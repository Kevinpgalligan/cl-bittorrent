(in-package bittorrent-test)

(def-suite client
  :in bittorrent)

(in-suite client)

(defparameter *num-pieces* 10)
(defparameter *curr-time* (* 5000 internal-time-units-per-second))
(defparameter *tracker-interval* 900)

(defun make-test-torrent ()
  ;; Can fill in the parameters as they become needed for unit tests.
  (make-instance 'torrent
                 :metainfo nil
                 :tracker-list nil
                 :info-hash nil
                 :dirname nil
                 :files nil
                 :total-length 100
                 :piece-length 10
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
   *tracker-interval*
   peer-states
   :time-now *curr-time*
   :peer-states peer-states))

(defmacro def-client-test (name &body body)
  `(test ,name
     (log:config :off)
     (with-dynamic-stubs ((get-time-now *curr-time*))
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
    (is (not (gethash 1 (bito::index->peer-state client))))
    (is (member 2 (peer-states client) :key #'index))
    (is (gethash 2 (bito::index->peer-state client)))))

(def-client-test prune-timed-out-requests
  (let* ((torrent (make-test-torrent))
         (client (make-test-client torrent nil)))
    (with-slots (bito::outstanding-requests) client
      (push (make-instance 'bito::outstanding-request
                           :peer-index 1
                           :piece-index 0
                           :block-begin-index 0
                           :len 100
                           :request-time 0)
            bito::outstanding-requests)
      (push (make-instance 'bito::outstanding-request
                           :peer-index 2
                           :piece-index 0
                           :block-begin-index 0
                           :len 100
                           :request-time *curr-time*)
            bito::outstanding-requests)
      (bito::prune-timed-out-piece-requests client)
      (is (not (member 1 bito::outstanding-requests :key #'bito::peer-index)))
      (is (member 2 bito::outstanding-requests :key #'bito::peer-index)))))

(def-client-test maybe-update-chokes-when-not-time
  (let* ((torrent (make-test-torrent))
         (client (make-test-client torrent nil)))
    (setf (bito::last-update-time client)
          (- *curr-time* (* 9 internal-time-units-per-second)))
    (with-dynamic-stubs ((bito::update-chokes nil))
      (bito::maybe-update-chokes client)
      (is (= 0 (mockingbird:call-times-for 'bito::update-chokes)))
      (is (= 0 (bito::optimistic-unchoke-count client))))))

(def-client-test maybe-update-chokes-when-is-time
  (let* ((torrent (make-test-torrent))
         (client (make-test-client torrent nil)))
    (setf (bito::last-update-time client)
          (- *curr-time* (* 11 internal-time-units-per-second)))
    (with-dynamic-stubs ((bito::update-chokes nil))
      (bito::maybe-update-chokes client)
      (is (= 1 (mockingbird:call-times-for 'bito::update-chokes)))
      (is (= 1 (bito::optimistic-unchoke-count client))))))

(def-client-test choke-update-single-peer
  (let* ((torrent (make-test-torrent))
         (ps (make-peer-state torrent *curr-time* 1))
         (peer-states
           (list ps))
         (client (make-test-client torrent peer-states)))
    (bito::update-chokes client)
    (is-false (bito::choked ps))
    (is (member ps
                (bito::pending-messages client)
                :key (lambda (msg)
                       (bito::target msg))))))

(defun make-peer-states (torrent data &key uploading)
  (loop for (id num-bytes) in data
        collect (let ((ps (make-peer-state torrent *curr-time* id)))
                  (if uploading
                      (bito::increment-uploaded-bytes ps num-bytes)
                      (bito::increment-downloaded-bytes ps num-bytes))
                  ps)))

(def-client-test choke-update-when-downloading
  (let ((torrent (make-test-torrent)))
    (destructuring-bind (ps1 ps2 ps3 ps4 ps5 ps6)
        (make-peer-states torrent
                          '((1 100)
                            (2 105)
                            (3 101)
                            (4 90)
                            (5 200)
                            (6 0)))
      (let* ((peer-states (list ps1 ps2 ps3 ps4 ps5 ps6))
             (client (make-test-client torrent peer-states)))
        (bito::update-chokes client)
        ;; Top 4 most generous peers should be unchoked.
        (is-false (bito::choked ps1))
        (is-false (bito::choked ps2))
        (is-false (bito::choked ps3))
        (is-false (bito::choked ps5))
        ;; One of the remaining peers should be optimistically unchoked.
        (is-true (or (and (not (bito::choked ps4)) (bito::choked ps6))
                     (and (bito::choked ps4) (not (bito::choked ps6)))))))))

(def-client-test choke-update-when-uploading
  (let ((torrent (make-test-torrent)))
    (destructuring-bind (ps1 ps2 ps3 ps4 ps5 ps6)
        (make-peer-states torrent
                          '((1 100)
                            (2 105)
                            (3 101)
                            (4 90)
                            (5 200)
                            (6 0))
                          :uploading t)
      (let* ((peer-states (list ps1 ps2 ps3 ps4 ps5 ps6))
             (client (make-test-client torrent peer-states)))
        (setf (bito::download-complete-p client) t)
        (bito::update-chokes client)
        ;; Top 4 most generous peers should be unchoked.
        (is-false (bito::choked ps1))
        (is-false (bito::choked ps2))
        (is-false (bito::choked ps3))
        (is-false (bito::choked ps5))
        ;; One of the remaining peers should be optimistically unchoked.
        (is-true (or (and (not (bito::choked ps4)) (bito::choked ps6))
                     (and (bito::choked ps4) (not (bito::choked ps6)))))))))

(defun find-pending-message (client target &optional id)
  (find (list :target target :id id)
        (bito::pending-messages client)
        :key (lambda (msg)
               (list :target (bito::target msg)
                     :id (and id (bito::message-id msg))))
        :test 'equalp))

(def-client-test optimistic-unchoke-and-choke-updates-sent-when-necessary
  (let ((torrent (make-test-torrent)))
    (destructuring-bind (ps1 ps2 ps3 ps4 ps5 ps6)
        (make-peer-states torrent
                          '((1 100)
                            (2 100)
                            (3 100)
                            (4 100)
                            (5 0)
                            (6 0)))
      (let* ((peer-states (list ps1 ps2 ps3 ps4 ps5 ps6))
             (client (make-test-client torrent peer-states)))
        (with-dynamic-stubs ((bito::get-random-element ps5))
          (bito::maybe-update-chokes client))
        (is-false (bito::choked ps5))
        (is-true (bito::choked ps6))
        (is-true (find-pending-message client ps5 :unchoke))
        (is-false (find-pending-message client ps6))
        ;; Now clear the pending peer messages.
        (setf (bito::pending-messages client) nil)
        (with-dynamic-stubs ((bito::get-random-element ps6))
          (flet ((inc-downloaded ()
                   (loop for ps in (list ps1 ps2 ps3 ps4)
                         do (bito::increment-downloaded-bytes ps 100))))
            ;; Basically, trigger 3 more choke updates, and ensure that the
            ;; new optimistic unchoke only takes place on the last update.
            (setf (bito::last-update-time client) 0)
            (inc-downloaded)
            ;; Mini check that transfer rates are updated.
            (bito::increment-uploaded-bytes ps1 50)
            (is (= 200 (bito::downloaded-bytes-sum ps1)))
            (bito::maybe-update-chokes client)
            (is (= 100 (bito::downloaded-bytes-sum ps1)))
            (is (= 50 (bito::uploaded-bytes-sum ps1)))
            (is-false (bito::choked ps5))
            (is-true (bito::choked ps6))
            (is-false (find-pending-message client ps5))
            (is-false (find-pending-message client ps6))

            (setf (bito::last-update-time client) 0)
            (inc-downloaded)
            (bito::maybe-update-chokes client)
            (is (= 0 (bito::uploaded-bytes-sum ps1)))
            (is-false (bito::choked ps5))
            (is-true (bito::choked ps6))
            (is-false (find-pending-message client ps5))
            (is-false (find-pending-message client ps6))

            (setf (bito::last-update-time client) 0)
            (inc-downloaded)
            (bito::maybe-update-chokes client)
            (is-true (bito::choked ps5))
            (is-false (bito::choked ps6))
            (is-true (find-pending-message client ps5 :choke))
            (is-true (find-pending-message client ps6 :unchoke))))))))

(def-client-test pings-tracker
  (let* ((torrent (make-test-torrent))
         (client (make-test-client torrent nil)))
    (setf (bito::last-tracker-ping client)
          (- *curr-time*
             (* internal-time-units-per-second *tracker-interval*)
             1))
    (with-dynamic-mocks (bito::query-tracker)
      (bito::maybe-ping-tracker client)
      (is (= 1 (mockingbird:call-times-for 'bito::query-tracker)))
      (mockingbird:clear-calls)
      (bito::maybe-ping-tracker client)
      (is (= 0 (mockingbird:call-times-for 'bito::query-tracker))))))

(defun verify-queue-message (msgs msg-tag msg-contents &key (count 1))
  (is (= count
         (length
          (remove-if-not
           (lambda (msg)
             (and (eq (bito::tag msg) msg-tag)
                  (or (and (eq (class-of (bito::contents msg))
                               (find-class 'bito::message))
                           (eq (class-of msg-contents)
                               (find-class'bito::message))
                           (bito::message= (bito::contents msg) msg-contents))
                      (equalp msg-contents (bito::contents msg)))))
           msgs)))))

(def-client-test sends-keepalives
  (let* ((torrent (make-test-torrent))
         (t2 (- *curr-time*
                bito::*max-idle-time*
                (- 1)))
         ;; Should send keepalive to first but not second.
         (ps1 (make-peer-state torrent
                               (- *curr-time*
                                  bito::*max-idle-time*
                                  1)
                               1))
         (ps2 (make-peer-state torrent t2 2))
         (client
           (make-test-client torrent (list ps1 ps2))))
    (bito::send-keepalives client)
    (is (= *curr-time* (bito::last-send-time ps1)))
    (is (=  t2 (bito::last-send-time ps2)))
    (is-true (bito::have-sent-p ps1))
    (is-false (bito::have-sent-p ps2))
    (let ((msgs (bito::qdump (bito::queue ps1))))
      (verify-queue-message msgs
                            :peer-message
                            (bito::make-message :id :keep-alive)))
    (is-true (bito::qempty-p (bito::queue ps2)))))

;;;; OTHER TESTS
;; 1. send proactive messages
;; 2. send pending messages
;; 3. send requested blocks
;; 4. connect to new peers
;; 5. process messages
