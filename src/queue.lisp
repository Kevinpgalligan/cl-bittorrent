;;;; For communication between threads.

(in-package bittorrent)

(defclass qmessage ()
  ((tag :initarg :tag :reader tag)
   (contents :initarg :contents :reader contents)
   (id :initarg :id
       :reader id
       :documentation "For letting someone know who sent a message.")))

(deftype queue-tag () '(member :peer-message :shutdown))

(defmethod print-object ((obj qmessage) stream)
  (format stream "#<QMESSAGE tag=~a contents=~a id=~a>"
          (tag obj) (contents obj) (id obj)))

(defun make-queue ()
  (lparallel.queue:make-queue))

(defun queue-message (&key tag contents id)
  (declare (queue-tag tag))
  (make-instance 'qmessage
                 :tag tag
                 :contents contents
                 :id id))

(defun qpush (queue message)
  (lparallel.queue:push-queue message queue))

(defun qpop (queue &optional (timeout 0))
  (lparallel.queue:try-pop-queue queue :timeout timeout))

(defun qempty-p (queue)
  (lparallel.queue:queue-empty-p queue))

(defun qdump (queue)
  (loop while (not (qempty-p queue))
        collect (qpop queue)))
