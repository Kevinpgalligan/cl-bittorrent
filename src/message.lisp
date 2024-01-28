;;;; Peer-to-peer messages and their buffering/parsing.

(in-package bittorrent)

;; 16384 bytes is the standard max request size, but leaving some
;; breathing room here.
(defparameter *message-buff-size* 18000)

(defclass message ()
  ((id :initarg :id :reader id)
   (data :initarg :data :initform nil :reader data)))

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

(defun parse-message (msg-buff len)
  (if (= len 0)
      (make-message :id :keep-alive)
      (let ((id (message-buffer-ref msg-buff 4)))
        (case id
          (0 (make-message :id :choke))
          (1 (make-message :id :unchoke))
          (2 (make-message :id :interested))
          (3 (make-message :id :not-interested))
          (4 (make-message :id :have :data (parse-have-message msg-buff)))
          (5 (make-message :id :bitfield :data (parse-bitfield-message msg-buff len)))
          (6 (make-message :id :request :data (parse-request-message msg-buff)))
          (7 (make-message :id :piece :data (parse-piece-message msg-buff)))
          (8 (make-message :id :cancel :data (parse-cancel-message msg-buff)))
          (t (error (format nil "Unknown message ID ~a." id)))))))

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
