;;;; For communication between threads.

(in-package bittorrent)

(defclass qmessage ()
  ((tag :initarg :tag :reader tag)
   (contents :initarg :contents :reader contents)))

(deftype queue-tag () '(member :peer-message :shutdown))

(defmethod print-object ((obj qmessage) stream)
  (format stream "#<QMESSAGE tag=~a contents=~a>" (tag obj) (contents obj)))

(defun make-queue ()
  (lparallel.queue:make-queue))

(defun queue-message (&key tag contents)
  (declare (queue-tag tag))
  (make-instance 'qmessage :tag tag :contents contents))

(defun qpush (queue message)
  (lparallel.queue:push-queue message queue))

(defun qpop (queue &optional (timeout 0))
  (lparallel.queue:try-pop-queue queue :timeout timeout))
