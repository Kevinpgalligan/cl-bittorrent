;; Used to generate a response for the dummy tracker.
;; The torrent file at data/for-tests.torrent directs to
;; 127.0.0.1:4242/announce, which is the address that the "tracker" should
;; be listening on.
;;   - (set-peer! port id) is used to set a peer to be included in the list.
;;     The port and ID should be findable from the logs of a running client.
;;     Note that the ID in the logs is URL-encoded, decode using:
;;       (quri:url-decode "URL-ENCODED-STRING-HERE" :encoding :latin1)
;;   - (clear-peer!) is self-explanatory.
;;   - (save-tracker-response) dumps the appropriate response somewhere.

(defparameter *response-path* "/tmp/tracker-response")
(defparameter *peer* nil)

;; Hello! If you're taking the URL-encoded peer ID from a client's logs, you
;; will want to use this code to decode it first:
;;    (quri:url-decode "URL-ENCODED-STRING-HERE" :encoding :latin1)
(defun set-peer! (port id)
  "ID is a string in the format expected by the tracker in the HTTP request."
  (setf *peer* (list :port port :id id)))

(defun clear-peer! ()
  (setf *peer* nil))

(defun bencoded-string (s)
  (format nil "~a:~a" (length s) s))

(defun bencoded-integer (i)
  (format nil "i~ae" i))

(defun get-peer-string ()
  (if (null *peer*)
      ""
      (concatenate
       'string
       "d"
       (bencoded-string "ip")
       (bencoded-string "127.0.0.1")
       (bencoded-string "peer id")
       (bencoded-string (getf *peer* :id))
       (bencoded-string "port")
       (bencoded-integer (getf *peer* :port))
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

(defun save-tracker-response ()
  (with-open-file (stream *response-path*
                          :direction :output
                          :if-exists :supersede
                          :external-format :latin1)
    (write-string (get-dummy-message) stream)))
