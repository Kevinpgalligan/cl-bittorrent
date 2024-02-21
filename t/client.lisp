(in-package bittorrent-test)

(def-suite client
  :in bittorrent)

(in-suite client)

(defparameter *num-pieces* 10)
(defparameter *curr-time* (* 5000 internal-time-units-per-second))

(defun make-test-torrent ()
  ;; Can fill in the parameters as they become needed for unit tests.
  (make-instance 'torrent
                 :metainfo nil
                 :tracker-list nil
                 :info-hash nil
                 :dirname nil
                 :files nil
                 :total-length nil
                 :piece-length nil
                 :piece-hashes nil
                 :num-pieces *num-pieces*))

(defun make-test-client (torrent &optional peer-states)
  (make-client
   nil ; id
   torrent
   4242
   "/tmp/"
   "http://tracker:80/announce"
   (make-array *num-pieces*
               :element-type 'bit
               :initial-element 0)
   nil ; listen sock
   (* 900 internal-time-units-per-second)
   peer-states
   :time-now *curr-time*
   :peer-states peer-states))

(defmacro def-client-test (name &body body)
  `(test ,name
     (log:config :off)
     (with-stubs ((get-time-now *curr-time*))
       ,@body)))

(def-client-test prunes-idle-peers
  (let* ((torrent (make-test-torrent))
         (peer-states
           ;; First peer should be pruned, second one shouldn't.
           (list (make-peer-state torrent 0 1)
                 (make-peer-state torrent *curr-time* 2)))
         (client (make-test-client torrent peer-states)))
    (prune-idle-peers client)
    (is (not (member 1 (peer-states client) :key #'index)))
    (is (member 2 (peer-states client) :key #'index))))
