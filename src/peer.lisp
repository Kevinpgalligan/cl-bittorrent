;;;; Communication with peers.

(in-package bittorrent)

(defparameter *socket-buffer-size* 65507)
(defparameter *socket-wait-timeout* 0.2)
(defparameter *queue-timeout* 0.2)

(define-condition ordered-to-close (condition) ())
(define-condition failed-handshake (condition) ())

(defclass peer ()
  ((ip :initarg :ip :accessor ip)
   (port :initarg :port :accessor port)
   (id :initarg :id :accessor id)))

(defun random-peer-id ()
  (coerce (loop repeat 20 collect (code-char (random 256)))
          'string))

(defun peer-loop (torrent id receive-queue send-queue &key peer sock)
  (when (and (null peer) (null sock))
    (error "Need one of PEER or SOCK."))
  (let ((socket-buffer (make-array *socket-buffer-size*
                                   :element-type '(unsigned-byte 8)
                                   :fill-pointer 0))
        (msg-buff (make-message-buffer (num-pieces torrent))))
    (handler-case
        (progn
          (when (null sock)
            (setf sock (usocket:socket-connect (ip peer) (port peer))))
          (send-handshake torrent id sock)
          (verify-handshake torrent peer sock socket-buffer)
          (message-loop send-queue receive-queue sock msg-buff socket-buffer))
      (condition (c)
        (when sock
          (usocket:socket-close sock))
        (qpush send-queue
               (queue-message :tag :shutdown
                              :contents (format nil "Closing due to: ~a" c)))))))

;;; There's a problem: on serialising to the array, we assume there's a
;;; fill pointer starting at 0, which is used to push bytes to the end of
;;; the array. usocket uses the length of the array (determined by the
;;; fill pointer) to determine the amount of data to read in (I think), so
;;; we can't always set the fill pointer to 0 after using the array. So these
;;; are some utility functions to handle the fill pointer on sending and
;;; receiving data.
(defun prepare-socket-buffer (socket-buffer)
  (setf (fill-pointer socket-buffer) (array-total-size socket-buffer)))

(defun clear-socket-buffer (socket-buffer)
  (setf (fill-pointer socket-buffer) 0))

(defun socket-buffer-receive (sock socket-buffer)
  (prepare-socket-buffer socket-buffer)
  (multiple-value-bind (buffer length)
      (usocket:socket-receive sock socket-buffer nil)
    (declare (ignorable buffer))
    (setf (fill-pointer socket-buffer) length)))

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

(defun verify-handshake (torrent peer sock socket-buffer)
  (socket-buffer-receive sock socket-buffer)
  (when (not (bytes-match? socket-buffer
                           (list *handshake-header*
                                 (info-hash torrent)
                                 (id peer))))
      (signal 'failed-handshake)))

(defun bytes-match? (buffer bytes-and-strings)
  (loop for i = 0
        for x in bytes-and-strings
        when (or (< (length buffer) (+ i (length x)))
                 (mismatch buffer
                           (if (stringp x) (flexi-streams:string-to-octets x) x)
                           :start1 i
                           :end1 (+ i (length x))))
          do (return nil)
        do (incf i (length x))
        finally (return (= i (length buffer)))))

(defun message-loop (send-queue receive-queue sock msg-buff socket-buffer)
  (loop do (read-from-peer sock send-queue msg-buff socket-buffer)
        do (execute-instructions receive-queue sock socket-buffer)))

(defun read-from-peer (sock send-queue msg-buff socket-buffer)
  (multiple-value-bind (ready-p time-remaining)
      (usocket:wait-for-input sock :timeout *socket-wait-timeout*)
    (declare (ignorable time-remaining))
    (when ready-p
      (loop for msg in (mb-store msg-buff
                                 (usocket:socket-receive sock socket-buffer nil))
            do (qpush send-queue
                      (queue-message :tag :peer-message
                                     :contents msg))))))

(defun execute-instructions (receive-queue sock socket-buffer)
  (loop for i = 0 then (1+ i)
        for instruction = (qpop receive-queue (if (= 0 i)
                                                  *queue-timeout*
                                                  0))
        while instruction
        do (handle-instruction instruction sock socket-buffer)))

(defun handle-instruction (instruction sock socket-buffer)
  (case (tag instruction)
    (:shutdown (signal 'ordered-to-close))
    (:peer-message
     (clear-socket-buffer socket-buffer)
     (serialise-message (contents instruction) socket-buffer)
     (usocket:socket-send sock socket-buffer (length socket-buffer)))))
