;; A dummy tracker, used for integration testing. Ignores HTTP
;; parameters and just returns a dummy result (no peers).
;; To run, call slime-eval-buffer from within Emacs.
;; set-peer! can be used to set a peer to be included in the list.
;; Note that data/for-tests.torrent directs to 127.0.0.1:4242/announce,
;; which is the address that this "tracker" listens on.

(ql:quickload 'hunchentoot)

(defparameter *peer* nil)

(defun set-peer! (port id)
  "ID is a string in the format expected by the tracker in the HTTP request."
  (list :port port :id id))

(defun bencoded-string (s)
  (format nil "~a:~a" (length s) s))

(defun get-peer-string ()
  (if (null *peer*)
      ""
      (concatenate
       'string
       "d"
       (bencoded-string "name")
       (bencoded-string "127.0.0.1")
       (bencoded-string "port")
       (bencoded-string (getf *peer* :port))
       (bencoded-string "peer id")
       (bencoded-string (getf *peer* :id))
       "e")))

(defun get-dummy-message ()
  (concatenate
   'string
   "d"
   "8:intervali900e"
   "10:tracker id5:hello"
   "8:completei0e"
   "10:incompletei0e"
   "5:peersl"
   (get-peer-string)
   "e"
   "e"))

;; Following an example from Hunchentoot docs:
;;   https://edicl.github.io/hunchentoot/#subclassing-acceptors
;; easy-acceptor wasn't working because it was trying to convert the
;; percent-encoded URL parameters to UTF-8 (they're bytestrings).
(defclass dummytracker (hunchentoot:acceptor)
  ((dispatch-table
    :initform '()
    :accessor dispatch-table))
  (:default-initargs
   :address "127.0.0.1"
   :port 4242))

(defmethod hunchentoot:acceptor-dispatch-request ((dummytracker dummytracker)
                                                  request)
  (mapc (lambda (dispatcher)
	  (let ((handler (funcall dispatcher request)))
	    (when handler
	      (return-from hunchentoot:acceptor-dispatch-request
                (funcall handler)))))
	(dispatch-table dummytracker))
  (call-next-method))

(defvar *dummy*
  (make-instance 'dummytracker
                 :name 'http
                 :port 4242
                 :access-log-destination nil
                 :message-log-destination nil))

(push
 (hunchentoot:create-prefix-dispatcher "/announce" 'get-dummy-message)
 (dispatch-table *dummy*))

(hunchentoot:start *dummy*)

;; Call this to stop the tracker server:
;;   (hunchentoot:stop *dummy*)
