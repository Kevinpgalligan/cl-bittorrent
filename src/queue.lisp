;;;; For communication between threads.

(in-package bittorrent)

(defclass qmessage ()
  ((tag :initarg :tag :reader tag)
   (contents :initarg :contents :reader contents)))

(defun make-queue ()
  (lparallel.queue:make-queue))

(defun queue-message (&key tag contents)
  (make-instance 'qmessage :tag tag :contents contents))

(defun qpush (queue message)
  (lparallel.queue:push-queue message queue))

(defun qpop (queue &optional (timeout 0))
  (lparallel.queue:try-pop-queue queue :timeout timeout))
