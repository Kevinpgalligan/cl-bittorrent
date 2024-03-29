;;;; Communication with peers.

(in-package bittorrent)

(defparameter *handshake-timeout* 5)
(defparameter *socket-wait-timeout* 0.5)
(defparameter *queue-timeout* 0.2)
(defparameter *id-num-bytes* 20)

(define-condition ordered-to-close (condition) ())
(define-condition failed-handshake (condition) ())

(defclass peer ()
  ((ip :initarg :ip :accessor ip)
   (port :initarg :port :accessor port)
   (id :initarg :id :accessor id)))

(defmethod print-object ((instance peer) stream)
  (format stream "#<PEER ~a:~a>" (ip instance) (port instance)))

(defun random-peer-id ()
  (coerce (loop repeat *id-num-bytes* collect (code-char (random 256)))
          'string))

(defun peer-loop (torrent id receive-queue send-queue peer-index
                  &key peer sock debug-p)
  (let ((msg-buff (make-message-buffer (num-pieces torrent))))
    (flet ((handle-condition (c)
             (when sock
               (usocket:socket-close sock))
             (qpush send-queue
                    (queue-message :tag :shutdown
                                   :contents (format
                                              nil
                                              "Closing due to: ~a~%====BACKTRACE:~%~a"
                                              c
                                              #+sbcl(sb-debug:list-backtrace)
                                              #-sbcl"(only with SBCL)")
                                   :id peer-index))
             (if debug-p
                 (error (format nil "Peer loop terminating because: ~a" c))
                 (progn
                   #+sbcl (sb-thread:return-from-thread 0)
                   #-sbcl(error "Dunno how to cleanly exit a thread in non-SBCL!")))))
      (handler-bind ((condition #'handle-condition))
        (progn
          (when (and (null peer) (null sock))
            (error "Need one of PEER or SOCK."))
          (when (null sock)
            (setf sock (usocket:socket-connect (ip peer)
                                               (port peer)
                                               :timeout *socket-wait-timeout*
                                               :element-type '(unsigned-byte 8))))
          (send-handshake torrent id sock)
          (verify-handshake torrent peer sock)
          (message-loop send-queue receive-queue sock msg-buff peer-index))))))

(defparameter *handshake-header*
  (concatenate 'vector
               #(19)
               (flexi-streams:string-to-octets "BitTorrent protocol") 
               #(0 0 0 0 0 0 0 0)))

(defun send-handshake (torrent id sock)
  (let ((stream (usocket:socket-stream sock)))
    (write-sequence *handshake-header* stream)
    (write-sequence (flexi-streams:string-to-octets (info-hash torrent)) stream)
    (write-sequence (flexi-streams:string-to-octets id) stream)
    (force-output stream)))

(defun verify-handshake (torrent peer sock)
  (usocket:wait-for-input sock :timeout *handshake-timeout*)
  (loop with stream = (usocket:socket-stream sock)
        for x in (list *handshake-header*
                       (info-hash torrent)
                       (if (and peer (not (null (id peer))))
                           (id peer)
                           *id-num-bytes*))
        do (if (integerp x)
               (skip-bytes stream x)
               (when (not (bytes-match? stream x))
                 (signal 'failed-handshake)))))

(defun skip-bytes (stream n)
  (loop repeat n do (read-byte stream)))

(defun bytes-match? (stream obj)
  (let ((transf (if (stringp obj) #'char-code #'identity)))
    (loop for x across obj
          when (not (listen stream))
            do (return nil)
          always (= (funcall transf x) (read-byte stream)))))

(defun message-loop (send-queue receive-queue sock msg-buff peer-index)
  (loop do (read-from-peer sock send-queue msg-buff peer-index)
        do (execute-instructions receive-queue sock)
           ;; Leaving this until after all the instructions have
           ;; been executed leaves the opportunity for buffering
           ;; messages and sending them all at once, but in practice
           ;; we're not doing that right now.
        do (force-output (usocket:socket-stream sock))))

(defun read-from-peer (sock send-queue msg-buff peer-index)
  (usocket:wait-for-input sock :timeout *socket-wait-timeout*)
  (loop for msg in (mb-store msg-buff (usocket:socket-stream sock))
        do (qpush send-queue
                  (queue-message :tag :peer-message
                                 :contents msg
                                 :id peer-index))))

(defun execute-instructions (receive-queue sock)
  (loop for i = 0 then (1+ i)
        for instruction = (qpop receive-queue
                                (if (zerop i) *queue-timeout* 0))
        while instruction
        do (handle-instruction instruction sock)))

(defun handle-instruction (instruction sock)
  (case (tag instruction)
    (:shutdown (signal 'ordered-to-close))
    (:peer-message
     (serialise-message (contents instruction) (usocket:socket-stream sock)))))
