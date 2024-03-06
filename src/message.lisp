;;;; Peer-to-peer messages and their buffering/parsing/serialising.

(in-package bittorrent)

(defparameter *max-request-size* 16384)
;; Leaving some breathing room here.
(defparameter *message-buff-size* 18000)
(defparameter *len-field-bytes* 4)

(defclass message ()
  ((id :initarg :id :reader id)
   (data :initarg :data :initform nil :accessor data)))

(deftype message-id ()
  '(member :choke :unchoke :interested
    :not-interested :have :bitfield
    :request :piece :cancel :keep-alive))

(defmethod print-object ((instance message) stream)
  (format stream "#<MESSAGE id=~a data={~a}>" (id instance) (data instance)))

(defun make-message (&key id data)
  (declare (message-id id))
  (make-instance 'message :id id :data data))

(defun message= (msg1 msg2)
  (and (eq (id msg1) (id msg2))
       (equalp (data msg1) (data msg2))))

(defclass message-buffer ()
  ((bytes :initarg :bytes :accessor bytes)
   (size :initarg :size :accessor size)
   (expected-length :initarg :expected-length :accessor expected-length)
   (num-pieces :initarg :num-pieces :accessor num-pieces)
   (bytes-count :initarg :bytes-count :accessor bytes-count)
   (read-pos :initarg :read-pos :accessor read-pos)))

(defun make-message-buffer (num-pieces &optional (size *message-buff-size*))
  (make-instance 'message-buffer
                 :bytes (make-array size :element-type '(unsigned-byte 8))
                 :size size
                 :expected-length nil
                 :num-pieces num-pieces
                 :bytes-count 0
                 :read-pos 0))

(defun clear-message-buffer (msg-buff)
  (setf (bytes-count msg-buff) 0)
  (setf (read-pos msg-buff) 0))

(defun message-buffer-ref (msg-buff i)
  (aref (bytes msg-buff) i))

(defun (setf message-buffer-ref) (value msg-buff i)
  (setf (aref (bytes msg-buff) i) value))

(defun mb-store (msg-buff stream)
  "Reads in received bytes from a peer, returns a list of any received messages."
  (with-slots (expected-length) msg-buff
    (loop with done = nil
          while (not done)
          do (setf done (try-read-in-message msg-buff stream))
          when (not done)
            ;; We have all the bytes for this message, parse it! Then empty
            ;; the buffer and get ready for the next message.
            collect (let ((msg (parse-message msg-buff
                                              (- expected-length *len-field-bytes*))))
                      (clear-message-buffer msg-buff)
                      (setf expected-length nil)
                      msg))))

(defun try-read-in-message (msg-buff stream)
  "Attempts to read a message into MSG-BUFF from bytestream STREAM.
Returns whether the stream was exhausted (T or NIL) before a complete
message could be read."
  (with-slots (expected-length) msg-buff
    (when (null expected-length)
      ;; We need to read in the length field before continuing.
      (read-in-bytes-up-to msg-buff stream *len-field-bytes*)
      (setf expected-length
            (if (< (bytes-count msg-buff) *len-field-bytes*)
                nil
                (+ *len-field-bytes* ; length doesn't include its own bytes
                   (loop for j from 0 upto 3 
                         for mul = (expt 256 (- 3 j)) ; big-endian
                         sum (* mul (message-buffer-ref msg-buff j)))))))
    (cond
      ((null expected-length)
       t)
      ((> expected-length (size msg-buff))
       (error "Message length too big."))
      (t
       (read-in-bytes-up-to msg-buff stream expected-length)
       (< (bytes-count msg-buff) expected-length)))))

(defun read-in-bytes-up-to (msg-buff stream end-pos)
  ;; I wanted to use READ-SEQUENCE, but that hangs if no
  ;; more bytes are available, rather than stopping. Even
  ;; this version is imperfect, because LISTEN will return NIL
  ;; if the peer closes the stream, even when there are still
  ;; bytes left to read, but I'm willing to accept that shortcoming.
  (loop while (and (listen stream)
                   (< (bytes-count msg-buff) end-pos))
        do (setf (message-buffer-ref msg-buff (bytes-count msg-buff))
                 (read-byte stream))
        do (incf (bytes-count msg-buff))))

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

(defun consume-byte (msg-buff)
  (let ((b (message-buffer-ref msg-buff (read-pos msg-buff))))
    (incf (read-pos msg-buff))
    b))

(defun consume-remaining-bytes (msg-buff)
  (let ((bs (subseq (bytes msg-buff)
                    (read-pos msg-buff)
                    (expected-length msg-buff))))
    (setf (read-pos msg-buff) (bytes-count msg-buff))
    bs))

(defun bytes-remaining-p (msg-buff)
  (and (expected-length msg-buff)
       (< (read-pos msg-buff) (expected-length msg-buff))))

(defun bytes-remaining (msg-buff)
  (- (expected-length msg-buff) (read-pos msg-buff)))

(defun parse-message (msg-buff len)
  (incf (read-pos msg-buff) 4) ; skip the length field
  (if (= len 0)
      (make-message :id :keep-alive)
      (let* ((id-num (consume-byte msg-buff))
             (id (num->id id-num))
             (msg (make-message :id id)))
        (when (null id)
          (error (format nil "Unknown message ID ~a." id-num)))
        (setf (data msg)
              (case id
                (:have (parse-have-message msg-buff))
                (:bitfield (parse-bitfield-message msg-buff))
                (:request (parse-request-message msg-buff))
                (:piece (parse-piece-message msg-buff))
                (:cancel (parse-cancel-message msg-buff))))
        msg)))

(defun parse-have-message (msg-buff)
  (consume-bigen-integer msg-buff))

(defun consume-bigen-integer (msg-buff)
  "Parses a 4-byte big-endian integer."
  ;; I'm sure there are libraries that handle this, oh well.
  (loop for j from 0 below 4
        sum (* (expt 256 (- 3 j))
               (consume-byte msg-buff))))

(defun parse-bitfield-message (msg-buff)
  (with-slots (num-pieces) msg-buff
    (let ((bitfield-size (* 8 (bytes-remaining msg-buff))))
      (when (not (and (>= bitfield-size num-pieces)
                      (< bitfield-size (+ 8 num-pieces))))
        (error "Bitfield is not the correct size."))
      (let ((bitfield (make-array num-pieces :element-type 'bit)))
        ;; I'm sure there's some way to initialize the bit-vector
        ;; directly from the bytes rather than looping over them...
        (loop while (bytes-remaining-p msg-buff)
              for byte = (consume-byte msg-buff)
              for base-index = 0 then (+ base-index 8)
              do (loop for j from 0 below 8
                       for shift = (- 7 j)
                       for bit-index = (+ base-index j)
                       while (< bit-index num-pieces)
                       do (setf (aref bitfield bit-index)
                                (logand 1 (ash byte (- shift))))))
        bitfield))))

(defun parse-request-message (msg-buff)
  (list :index (consume-bigen-integer msg-buff)
        :begin (consume-bigen-integer msg-buff)
        :length (consume-bigen-integer msg-buff)))

(defun parse-piece-message (msg-buff)
  (list :index (consume-bigen-integer msg-buff)
        :begin (consume-bigen-integer msg-buff)
        :block (consume-remaining-bytes msg-buff)))

(defun parse-cancel-message (msg-buff)
  (parse-request-message msg-buff)) ; has the same payload as a request!

(defun serialise-message (msg stream)
  "Writes the serialised form of MSG to STREAM (a bytestream)."
  (let* ((id (id msg))
         (idnum (id->num id))
         (data (data msg)))
    (cond
      ((eq :keep-alive id)
       (write-bigen-integer stream 0))
      ((member id '(:choke :unchoke :interested :not-interested))
       (write-bigen-integer stream 1)
       (write-byte idnum stream))
      ((eq id :have)
       (write-bigen-integer stream 5)
       (write-byte idnum stream)
       (write-bigen-integer stream data))
      ((member id '(:request :cancel))
       (write-bigen-integer stream 13)
       (write-byte idnum stream)
       (write-bigen-integer stream (getf data :index))
       (write-bigen-integer stream (getf data :begin))
       (write-bigen-integer stream (getf data :length)))
      ((eq id :bitfield)
       (write-bigen-integer stream (1+ (calc-bitvec-length-in-bytes data)))
       (write-byte idnum stream)
       (write-bit-vector stream data))
      ((eq id :piece)
       (write-bigen-integer stream (+ 9 (length (getf data :block))))
       (write-byte idnum stream)
       (write-bigen-integer stream (getf data :index))
       (write-bigen-integer stream (getf data :begin))
       (write-sequence (getf data :block) stream))
      (t (error "Unknown piece type.")))))

(defun write-bigen-integer (stream value &key (bytes 4))
  (loop for i from (1- bytes) downto 0
        for mul = (expt 256 i)
        do (write-byte (floor value mul) stream)
        do (setf value (mod value mul))))

(defun calc-bitvec-length-in-bytes (bv)
  (ceiling (length bv) 8))

(defun write-bit-vector (stream bv)
  (loop for i = 0 then (1+ i)
        for bit-i = (* 8 i)
        while (< bit-i (length bv))
        do (write-byte (bv-extract-byte bv bit-i) stream)))

(defun bv-extract-byte (bv start-index)
  (loop with result = 0
        for i = 0 then (1+ i)
        for j = (+ i start-index)
        while (< i 8)
        ;; Piece 0 corresponds to the high bit of the first
        ;; byte. Bitfield layout is...
        ;;   0 0 0 0 0 0 0 0 | 0 0 0 0 0 0 0 0 | ...
        ;;   ^             ^
        ;;   piece 0       piece 8
        ;;   high bit      low bit
        ;; So we need to keep shifting left, even if we've reached
        ;; the end of the bit vector, so that the high bit corresponds
        ;; to the first piece.
        do (setf result (logior (ash result 1) 
                                (if (>= j (length bv))
                                    0
                                    (aref bv j))))
        finally (return result)))
