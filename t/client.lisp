(in-package bittorrent-test)

(def-suite client
  :in bittorrent)

(in-suite client)

(defparameter *num-pieces* 10)
(defparameter *curr-time* (* 5000 internal-time-units-per-second))

(defun make-test-client (&optional peers)
  (make-client
   nil ; id
   nil ; torrent
   4242
   "/tmp/"
   "http://tracker:80/announce"
   (make-array *num-pieces*
               :element-type 'bit
(run! 'prunes-idle-peers)
               :initial-element 0)
   nil ; listen sock
   (* 900 internal-time-units-per-second)
   peers
   :time-now *curr-time*))

(defmacro def-client-test (name &body body)
  `(test ,name
     (with-stubs ((get-time-now *curr-time*))
       (with-method-stubs ((num-pieces ((torrent nil)) *num-pieces*))
         ,@body))))

(def-client-test prunes-idle-peers
  (let* ((peers
           ;; First peer should be pruned, second one shouldn't.
           (list (make-peer-state nil 0 1)
                 (make-peer-state nil *curr-time* 2)))
         (client (make-test-client peers)))
    (prune-idle-peers client)
    (is (not (member 1 (peer-states client) :key #'index)))
    (is (member 2 (peer-states client) :key #'index))))
