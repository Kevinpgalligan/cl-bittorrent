;;;; Peer-to-peer messages and their buffering/parsing/serialising.

(in-package bittorrent)

;; 16384 bytes is the standard max request size, but leaving some
;; breathing room here.
(defparameter *message-buff-size* 18000)

(defclass message ()
  ((id :initarg :id :reader id)
   (data :initarg :data :initform nil :accessor data)))

(defmethod print-object ((instance message) stream)
  (format stream "#<MESSAGE id=~a data={~a}>" (id instance) (data instance)))

(defun make-message (&rest r)
  (apply #'make-instance 'message r))

(defun message= (msg1 msg2)
  (and (eq (id msg1) (id msg2))
       (equalp (data msg1) (data msg2))))

(defclass message-buffer ()
  ((bytes :initarg :bytes :accessor bytes)
   (size :initarg :size :accessor size)
   (expected-length :initarg :expected-length :accessor expected-length)
   (num-pieces :initarg :num-pieces :accessor num-pieces)))

(defun make-message-buffer (num-pieces &optional (size *message-buff-size*))
  (make-instance 'message-buffer
                 :bytes (make-array size
                                    :element-type '(unsigned-byte 8)
                                    :fill-pointer 0)
                 :size size
                 :expected-length nil
                 :num-pieces num-pieces))

(defun clear-message-buffer (msg-buff)
  (setf (fill-pointer (bytes msg-buff)) 0))

(defun bytes-count (msg-buff)
  (fill-pointer (bytes msg-buff)))

(defun message-buffer-ref (msg-buff i)
  (aref (bytes msg-buff) i))

(defun mb-store (msg-buff bytes)
  "Reads in received bytes from a peer, returns a list of any received messages."
  (with-slots (expected-length) msg-buff
    (loop with i = 0
          while (< i (length bytes))
          ;; We're at the start of a new message! Figure out how long it is.
          when (null expected-length)
            do (let ((len (parse-message-len msg-buff bytes i)))
                 ;; The "new expected length" might actually be nil. It's possible
                 ;; that fewer than 4 bytes of the next message were received, and
                 ;; hence we can't even tell how long it will be.
                 (when len
                   (if (> len (size msg-buff))
                       (error "Message length too large.")
                       ;; Add 4 because the length field doesn't include the bytes in
                       ;; the length field itself.
                       (setf expected-length (+ 4 len)))))
          do (incf i ; Read as much of the message into the buffer as possible!
                   (read-in-bytes (and expected-length
                                       (- expected-length (bytes-count msg-buff)))
                                  msg-buff
                                  bytes
                                  i))
          when (and expected-length (= (bytes-count msg-buff) expected-length))
            ;; We have all the bytes for this message, parse it! Then empty
            ;; the buffer and get ready for the next message. Subtract 4 from expected
            ;; length to get the value of the 'len' field of the message.
            collect (let ((msg (parse-message msg-buff (- expected-length 4))))
                      (clear-message-buffer msg-buff)
                      (setf expected-length nil)
                      msg))))

(defun parse-message-len (msg-buff bytes i)
  "Computes the expected length of the next message.
It's possible that the 'length' bytes are split between bytes we've already
received and newly-received bytes, e.g. if at the tail-end of a packet
we get 3 of the 4 'length' bytes. So need to consider both.
If all of the 'length' bytes haven't been received, returns nil." 
  (let ((num-bytes-already (bytes-count msg-buff)))
    (if (> 4 (+ num-bytes-already (- (length bytes) i)))
        nil ; not enough bytes to make up the 'len' field
        (loop for j from 0 upto 3 
              for mul = (expt 256 (- 3 j)) ; big-endian
              sum (* mul
                     (if (< j num-bytes-already)
                         (message-buffer-ref msg-buff j)
                         (aref bytes (+ i (- j num-bytes-already)))))))))

(defun read-in-bytes (num-bytes msg-buff bytes i)
  "Reads NUM-BYTES bytes from BYTES into the message buffer MSG-BUFF, starting
from index I in BYTES. If NUM-BYTES is nil, it reads all of BYTES into MSG-BUFF.
Note: it is assumed that, if NUM-BYTES is nil, there's enough space in MSG-BUFF.
Returns the number of bytes read."
  (let* ((bytes-to-read (or num-bytes (- (length bytes) i)))
         (final-index (+ i bytes-to-read)))
    (loop for j from i below final-index
          do (vector-push (aref bytes j) (bytes msg-buff)))
    bytes-to-read))

(defparameter *ids*
  '((0 . :choke)
    (1 . :unchoke)
    (2 . :interested)
    (3 . :not-interested)
    (4 . :have)
    (5 . :bitfield)
    (6 . :request)
    (7 . :piece)
    (8 . :cancel)))

(defun num->id (n) (cdr (assoc n *ids*)))
(defun id->num (id) (car (rassoc id *ids*)))

(defun parse-message (msg-buff len)
  (if (= len 0)
      (make-message :id :keep-alive)
      (let* ((id-num (message-buffer-ref msg-buff 4))
             (id (num->id id-num))
             (msg (make-message :id id)))
        (when (null id)
          (error (format nil "Unknown message ID ~a." id-num)))
        (setf (data msg)
              (case id
                (:have (parse-have-message msg-buff))
                (:bitfield (parse-bitfield-message msg-buff len))
                (:request (parse-request-message msg-buff))
                (:piece (parse-piece-message msg-buff))
                (:cancel (parse-cancel-message msg-buff))))
        msg)))

(defun parse-have-message (msg-buff)
  (parse-bigen-integer msg-buff 5))

(defun parse-bigen-integer (msg-buff i)
  "Parses a 4-byte big-endian integer."
  ;; I'm sure there are libraries that handle this, oh well.
  (loop for j from 0 below 4
        sum (* (expt 256 (- 3 j))
               (message-buffer-ref msg-buff (+ i j)))))

(defun parse-bitfield-message (msg-buff len)
  (with-slots (num-pieces) msg-buff
    ;; Subtract 1 byte for the length field.
    (let ((bitfield-size (* 8 (1- len))))
      (when (not (and (>= bitfield-size num-pieces)
                      (< bitfield-size (+ 8 num-pieces))))
        (error "Bitfield is not the correct size."))
      (let ((bitfield (make-array num-pieces :element-type 'bit)))
        ;; I'm sure there's some way to initialize the bit-vector
        ;; directly from the bytes rather than looping over them...
        (loop for i from 5 below (bytes-count msg-buff)
              for byte = (message-buffer-ref msg-buff i)
              do (loop for j from 0 below 8
                       for shift = (- 7 j)
                       for bit-index = (+ (* (- i 5) 8) j)
                       while (< bit-index num-pieces)
                       do (setf (aref bitfield bit-index)
                                (logand 1 (ash byte (- shift))))))
        bitfield))))

(defun parse-request-message (msg-buff)
  (list :index (parse-bigen-integer msg-buff 5)
        :begin (parse-bigen-integer msg-buff 9)
        :length (parse-bigen-integer msg-buff 13)))

(defun parse-piece-message (msg-buff)
  (list :index (parse-bigen-integer msg-buff 5)
        :begin (parse-bigen-integer msg-buff 9)
        :block (subseq (bytes msg-buff) 13)))

(defun parse-cancel-message (msg-buff)
  (parse-request-message msg-buff)) ; has the same payload as a request!

(defun serialise-message (msg buffer)
  "Writes the serialised form of MSG to BUFFER (a byte array)."
  (let* ((id (id msg))
         (idnum (id->num id))
         (data (data msg)))
    (cond
      ((eq :keep-alive id)
       (push-bigen-integer buffer 0)
       buffer)
      ((member id '(:choke :unchoke :interested :not-interested))
       (push-bigen-integer buffer 1)
       (push-bigen-integer buffer idnum :bytes 1))
      ((eq id :have)
       (push-bigen-integer buffer 5)
       (push-bigen-integer buffer idnum :bytes 1)
       (push-bigen-integer buffer data))
      ((member id '(:request :cancel))
       (push-bigen-integer buffer 13)
       (push-bigen-integer buffer idnum :bytes 1)
       (push-bigen-integer buffer (getf data :index))
       (push-bigen-integer buffer (getf data :begin))
       (push-bigen-integer buffer (getf data :length)))
      ((eq id :bitfield)
       (push-bigen-integer buffer (1+ (calc-bitvec-length-in-bytes data)))
       (push-bigen-integer buffer idnum :bytes 1)
       (push-bit-vector buffer data))
      ((eq id :piece)
       (push-bigen-integer buffer (+ 9 (length (getf data :block))))
       (push-bigen-integer buffer idnum :bytes 1)
       (push-bigen-integer buffer (getf data :index))
       (push-bigen-integer buffer (getf data :begin))
       (push-block buffer (getf data :block)))
      (t (error "Unknown piece type.")))))

(defun push-bigen-integer (buffer value &key (bytes 4))
  (incf (fill-pointer buffer) bytes)
  (loop for i below bytes
        ;; Loops from least-significant byte to most-significant, mod captures
        ;; the value of that byte and the shift moves it.
        do (setf (aref buffer (- (fill-pointer buffer) 1 i)) (mod value 256)
                 value (ash value (- 8)))))

(defun push-block (buffer block)
  (loop for byte across block
        do (vector-push-extend byte buffer)))

(defun calc-bitvec-length-in-bytes (bv)
  (ceiling (length bv) 8))

(defun push-bit-vector (buffer bv)
  (loop for i = 0 then (1+ i)
        for bit-i = (* 8 i)
        while (< bit-i (length bv))
        do (vector-push-extend (bv-extract-byte bv bit-i) buffer)))

(defun bv-extract-byte (bv start-index)
  (loop with result = 0
        for i = 0 then (1+ i)
        for j = (+ i start-index)
        while (and (< i 8)
                   (< j (length bv)))
        do (setf result (logior (ash result 1) (aref bv j)))
        finally (return result)))
