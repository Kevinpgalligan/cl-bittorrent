(in-package bittorrent-test)

(def-suite client
  :in bittorrent)

(in-suite client)

(defparameter *num-pieces* 10)
(defparameter *curr-time* (* 5000 internal-time-units-per-second))
(defparameter *tracker-interval* 900)
(defparameter *blocksize* 40)
(defparameter *piecesize* 120) ; 3 blocks per piece
(defparameter *client-id* 'i-am-an-id)

(defun make-test-torrent ()
  (make-instance 'torrent
                 :metainfo nil
                 :tracker-list nil
                 :info-hash nil
                 :dirname nil
                 :files nil
                 :total-length (* *piecesize* *num-pieces*)
                 :piece-length *piecesize*
                 :piece-hashes nil
                 :num-pieces *num-pieces*
                 :max-block-size *blocksize*))

(defun make-test-client (torrent &optional peer-states)
  (make-client
   *client-id*
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
  (loop for (id num-bytes interested) in data
        collect (let ((ps (make-peer-state torrent *curr-time* id)))
                  (setf (bito::they-interested ps) interested)
                  (if uploading
                      (bito::increment-uploaded-bytes ps num-bytes)
                      (bito::increment-downloaded-bytes ps num-bytes))
                  ps)))

(def-client-test choke-update-when-downloading
  (let ((torrent (make-test-torrent)))
    (destructuring-bind (ps1 ps2 ps3 ps4 ps5 ps6 ps7)
        (make-peer-states torrent
                          '((1 100 t)
                            (2 105 t)
                            (3 101 nil)
                            (4 90 t)
                            (5 200 t)
                            (6 0 t)
                            (7 10 t)))
      (let* ((peer-states (list ps1 ps2 ps3 ps4 ps5 ps6 ps7))
             (client (make-test-client torrent peer-states)))
        (with-dynamic-stubs ((bito::get-random-element ps6))
          (bito::update-chokes client))
        ;; Top 4 most generous (interested) peers should be unchoked.
        ;; And the optimistic unchoke.
        (is-false (bito::choked ps1))
        (is-false (bito::choked ps2))
        (is-false (bito::choked ps4))
        (is-false (bito::choked ps5))
        (is-false (bito::choked ps6))

        (is-true (bito::choked ps3))
        (is-true (bito::choked ps7))))))

(def-client-test choke-update-when-uploading
  (let ((torrent (make-test-torrent)))
    (destructuring-bind (ps1 ps2 ps3 ps4 ps5 ps6)
        (make-peer-states torrent
                          '((1 100 t)
                            (2 105 t)
                            (3 101 t)
                            (4 90 t)
                            (5 200 t)
                            (6 0 t))
                          :uploading t)
      (let* ((peer-states (list ps1 ps2 ps3 ps4 ps5 ps6))
             (client (make-test-client torrent peer-states)))
        (setf (bito::download-complete-p client) t)

        (with-dynamic-stubs ((bito::get-random-element ps6))
          (bito::update-chokes client))

        (is-false (bito::choked ps1))
        (is-false (bito::choked ps2))
        (is-false (bito::choked ps3))
        (is-true (bito::choked ps4))
        (is-false (bito::choked ps5))
        (is-false (bito::choked ps6))))))

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
                          '((1 100 t)
                            (2 100 t)
                            (3 100 t)
                            (4 100 t)
                            (5 0 t)
                            (6 0 t)))
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

(def-client-test sends-bitfield-on-start
  (let* ((torrent (make-test-torrent))
         (ps1 (make-peer-state torrent 0 1))
         (ps2 (make-peer-state torrent 0 2))
         (client (make-test-client torrent (list ps1 ps2))))
    ;; Won't send if we haven't downloaded any bytes.
    (incf (bito::downloaded-bytes client) 10)
    (setf (bito::have-sent-p ps1) t)
    (bito::maybe-send-bitfield client)
    (is-true (bito::qempty-p (bito::queue ps1)))
    (verify-queue-message (bito::qdump (bito::queue ps2))
                          :peer-message
                          (bito::make-message :id :bitfield
                                              :data (bito::piecemap client)))
    (bito::maybe-send-bitfield client)
    (is-true (bito::qempty-p (bito::queue ps1)))
    (is-true (bito::qempty-p (bito::queue ps2)))))

(defun some-bytes (n)
  (make-array n
              :element-type '(unsigned-byte 8)
              :initial-element 1))

(defun gather-sent-request-messages (&rest peer-states)
  (apply #'append
         (loop for ps in peer-states
               collect (mapcar
                        (lambda (qmsg)
                          (list (bito::index ps)
                                (bito::data (bito::contents qmsg))))
                        (remove-if-not
                         (lambda (qmsg)
                           (and (eq :peer-message (bito::tag qmsg))
                                (bito::contents qmsg)
                                (eq :request
                                    (bito::id (bito::contents qmsg)))))
                         (bito::qdump (bito::queue ps)))))))

(def-client-test remaining-blocks
  (let* ((torrent (make-test-torrent))
         (client (make-test-client torrent nil)))
    (let ((pp (bito::make-partial-piece 0 0 *piecesize*))
          (pp2 (bito::make-partial-piece 1 *piecesize* (* 2 *piecesize*))))
      (bito::block-insert pp (bito::make-block 0 0 (some-bytes *blocksize*)))
      (bito::save-partial-piece client pp)
      (push (make-instance 'bito::outstanding-request
                           :peer-index 2
                           :piece-index 1
                           :block-begin-index *blocksize*
                           :len *blocksize*
                           :request-time *curr-time*)
            (bito::outstanding-requests client))
      (is (equalp
           '((:piece-index 0
              :begin 40
              :length 40)
             (:piece-index 0
              :begin 80
              :length 40))
           (bito::remaining-blocks client pp)))
      (is (equalp
           '((:piece-index 1
              :begin 0
              :length 40)
             (:piece-index 1
              :begin 80
              :length 40))
           (bito::remaining-blocks client pp2))))))

(def-client-test sends-piece-requests
  (let* ((torrent (make-test-torrent))
         (ps1 (make-peer-state torrent 0 1))
         (ps2 (make-peer-state torrent 0 2))
         (ps3 (make-peer-state torrent 0 3))
         (client (make-test-client torrent (list ps1 ps2 ps3))))
    (setf (bito::they-choking ps2) nil
          (bito::they-choking ps3) nil)
    (bito::mark-piece ps1 0)
    (bito::mark-piece ps2 0)
    (bito::mark-piece ps2 1)
    (bito::mark-piece ps2 8)
    (bito::mark-piece ps3 2)
    ;; Let's say we've accumulated the first block of the piece
    ;; at index 1, and there's an outstanding request for the
    ;; second block. Each piece in the test env consists of 3 blocks.
    ;; The final block should be prioritised over the blocks of
    ;; other pieces because this piece is in progress.
    ;; Peer 2 is using 1/4 of its max concurrent requests; if we
    ;; didn't prioritise partial pieces, then (depending on the piece
    ;; iteration order) the remaining 3/4 requests would be used to
    ;; fetch piece 0.
    (let ((pp (bito::make-partial-piece 1 0 *piecesize*)))
      (bito::block-insert pp
                          (bito::make-block 1 0 (some-bytes *blocksize*)))
      (bito::save-partial-piece client pp))
    (push (make-instance 'bito::outstanding-request
                         :peer-index 2
                         :piece-index 1
                         :block-begin-index *blocksize*
                         :len *blocksize*
                         :request-time *curr-time*)
          (bito::outstanding-requests client))
    (bito::send-piece-requests client)
    (let ((msgs (gather-sent-request-messages ps1 ps2 ps3)))
      (flet ((num-messages-to-peer (index)
               (length
                (remove-if-not (lambda (msg) (= index (first msg)))
                               msgs))))
        (is (= 0 (num-messages-to-peer 1)))
        (is (= 4 (num-messages-to-peer 2)))
        (is (= 3 (num-messages-to-peer 3)))
        (is-true (find-if (lambda (data)
                            (and (= 1 (getf data :index))
                                 (= 80 (getf data :begin))
                                 (= 40 (getf data :length))))
                          (mapcar #'second msgs)))
        ;; Each block/request msg must correspond to an entry in the list
        ;; of outstanding requests.
        (loop for msg in msgs
              for peer-index = (first msg)
              for data = (second msg)
              do (is-true (find-if
                           (lambda (req)
                             (and
                              (= peer-index (bito::peer-index req))
                              (= (getf data :index) (bito::piece-index req))
                              (= (getf data :begin) (bito::block-begin-index req))
                              (= (getf data :length) (bito::len req))))
                           (bito::outstanding-requests client))))))))

(def-client-test sends-pending-messages-to-all
  (let* ((torrent (make-test-torrent))
         (ps1 (make-peer-state torrent 0 1))
         (ps2 (make-peer-state torrent 0 2))
         (client (make-test-client torrent (list ps1 ps2))))
    (bito::prepare-peer-message client :all :have 5)
    (bito::send-pending-messages client)
    (loop for ps in (list ps1 ps2)
          do (let ((qmsgs (bito::qdump (bito::queue ps))))
               (is (= 1 (length qmsgs)))
               (let ((qmsg (first qmsgs)))
                 (is (eq :peer-message (bito::tag qmsg)))
                 (is-true
                  (bito::message=
                   (bito::make-message :id :have :data 5)
                   (bito::contents qmsg))))))))

(def-client-test sends-pending-messages-to-single-peer
  (let* ((torrent (make-test-torrent))
         (ps1 (make-peer-state torrent 0 1))
         (ps2 (make-peer-state torrent 0 2))
         (client (make-test-client torrent (list ps1 ps2))))
    (bito::prepare-peer-message client ps1 :have 5)
    (bito::send-pending-messages client)
    (is-true (bito::qempty-p (bito::queue ps2)))
    (let ((qmsgs (bito::qdump (bito::queue ps1))))
      (is (= 1 (length qmsgs)))
      (let ((qmsg (first qmsgs)))
        (is (eq :peer-message (bito::tag qmsg)))
        (is-true
         (bito::message=
          (bito::make-message :id :have :data 5)
          (bito::contents qmsg)))))))

(defun verify-peer-messages (ps messages)
  (let ((qmsgs (bito::qdump (bito::queue ps))))
    (is (= (length messages) (length qmsgs)))
    (loop for qmsg in qmsgs
          for msg in messages
          do (progn
               (is (eq :peer-message (bito::tag qmsg)))
               (is-true
                (bito::message= msg (bito::contents qmsg)))))))

(def-client-test sends-requested-blocks
  (let* ((torrent (make-test-torrent))
         (ps1 (make-peer-state torrent 0 1))
         (client (make-test-client torrent (list ps1)))
         (bdata #(1 2 3 4 5 6 7 8 9 10)))
    (bito::add-to-requests-list client
                                ps1
                                '(:index 2
                                  :begin 0
                                  :length 10))
    (with-dynamic-stubs
        ((bito::load-bytes-from-files bdata))
      (bito::send-requested-blocks client))
    (verify-peer-messages
     ps1
     (list (bito::make-message :id :piece
                               :data (list
                                      :index 2
                                      :begin 0
                                      :block bdata))))
    (is (= 10 (bito::uploaded-bytes client)))
    (is (= 10 (bito::uploaded-bytes-sum ps1)))))

(def-client-test connects-to-new-peers
  (let* ((torrent (make-test-torrent))
         (client (make-test-client torrent
                                   (list (make-peer-state torrent
                                                          *curr-time*
                                                          1)))))
    (with-dynamic-stubs
        ((usocket:wait-for-input t)
         (usocket:socket-accept 'hello-fellow-sockets)
         (bito::spin-up-peer-thread nil))
      (bito::connect-to-new-peers client)
      (is (= 2 (length (bito::peer-states client))))
      (let ((ps (first (bito::peer-states client))))
        (is (= 2 (bito::index ps)))
        ;; Not sure if this is the place to test all these attributes, oh well.
        (is (eq t (bito::choked ps)))
        (is (eq t (bito::they-choking ps)))
        (is (eq nil (bito::interested ps)))
        (is (eq nil (bito::they-interested ps)))
        (is (= *num-pieces* (length (bito::piecemap ps))))
        (is (= 0 (bito::num-desired-pieces ps)))
        (is (= *curr-time* (bito::last-receive-time ps)))
        (is (= *curr-time* (bito::last-send-time ps)))
        (is (eq nil (bito::have-sent-p ps)))
        (is (= 0 (bito::downloaded-bytes-sum ps)))
        (is (= 0 (bito::uploaded-bytes-sum ps)))

        (is (= 3 (bito::next-peer-index client)))
        (is (eq ps (bito::gethash 2 (bito::index->peer-state client)) ))
        (is
         (equalp
          (list torrent
                *client-id*
                ps
                (bito::control-queue client)
                :sock 'hello-fellow-sockets)
          (mockingbird:nth-mock-args-for 1 'bito::spin-up-peer-thread)))))
    (with-dynamic-stubs
        ((usocket:wait-for-input nil)
         (bito::spin-up-peer-thread nil))
      (bito::connect-to-new-peers client)
      (is (= 2 (length (bito::peer-states client))))
      (is (= 0 (mockingbird:call-times-for 'bito::spin-up-peer-thread))))))

(def-client-test handles-shutdown-message
  (let* ((torrent (make-test-torrent))
         (ps (make-peer-state torrent *curr-time* 1))
         (client (make-test-client torrent (list ps)))
         (q (bito::control-queue client)))
    (push (make-instance 'bito::outstanding-request
                         :peer-index 1
                         :piece-index 0
                         :block-begin-index 0
                         :len 100
                         :request-time *curr-time*)
          (bito::outstanding-requests client))
    (bito::qpush q
                 (bito::queue-message :tag :shutdown
                                      :contents nil
                                      :id 1))
    (bito::process-messages client)
    (is-false (bito::peer-states client))
    (is-false (bito::outstanding-requests client))))

(defmacro def-message-test (name &body body)
  `(def-client-test ,name
     (let* ((torrent (make-test-torrent))
            (ps1 (make-peer-state torrent 0 1))
            (ps2 (make-peer-state torrent 0 2))
            (ps3 (make-peer-state torrent 0 3))
            (client (make-test-client torrent (list ps1 ps2 ps3)))
            (q (bito::control-queue client)))
       ,@body)))

(defun push-msg (q peer-index msg-id &optional msg-data)
  (bito::qpush q
               (bito::queue-message :tag :peer-message
                                    :contents (make-message :id msg-id
                                                            :data msg-data)
                                    :id peer-index)))

(def-message-test handles-keep-alive
  (push-msg q 1 :keep-alive)
  (bito::process-messages client)
  (is-false (bito::first-contact-p ps1))
  (is-true (bito::first-contact-p ps2))
  (is (= *curr-time* (bito::last-receive-time ps1)))
  (is (= 0 (bito::last-receive-time ps2))))

(def-message-test handles-choke-unchoke
  (setf (bito::they-choking ps2) nil)
  (push-msg q 1 :unchoke)
  (push-msg q 2 :choke)
  (push (make-instance 'bito::outstanding-request
                       :peer-index 2
                       :piece-index 0
                       :block-begin-index 0
                       :len 100
                       :request-time *curr-time*)
        (bito::outstanding-requests client))

  (bito::process-messages client)

  (is-false (bito::they-choking ps1))
  (is-true (bito::they-choking ps2))
  (is-false (bito::outstanding-requests client)))

(def-message-test handles-interested-not-interested
  (setf (bito::they-interested ps2) t)
  (push-msg q 1 :interested)
  (push-msg q 2 :not-interested)

  (bito::process-messages client)

  (is-true (bito::they-interested ps1))
  (is-false (bito::they-interested ps2)))

(defun verify-pending-messages (client exp-msgs)
  (let ((msgs (bito::pending-messages client)))
    (is (= (length exp-msgs) (length msgs)))
    (loop for msg in msgs
          for exp-msg in exp-msgs
          do (progn
               (is (eq (bito::target exp-msg) (bito::target msg)))
               (is (eq (bito::message-id exp-msg)
                       (bito::message-id msg)))
               (is (equalp (bito::data exp-msg)
                           (bito::data msg)))))))

(def-message-test handles-haves
  (bito::mark-piece client 0)
  (push-msg q 1 :have 0)
  (setf (bito::interested ps2) t)
  (bito::set-piecemap client ps2 #*1011010000)
  (push-msg q 2 :have 1)
  (push-msg q 3 :have 1)

  (bito::process-messages client)

  (is-false (bito::interested ps1))
  (is-true (bito::interested ps2))
  (is-true (bito::interested ps3))
  (is (equalp #*1000000000 (bito::piecemap ps1)))
  (is (equalp #*1111010000 (bito::piecemap ps2)))
  (is (equalp #*0100000000 (bito::piecemap ps3)))
  (verify-pending-messages
   client
   (list (make-instance 'bito::pending-peer-message
                        :target ps3
                        :message-id :interested
                        :data nil))))

(def-message-test handles-bitfields
  ;; This peer is sending their bitfield too late, so we ignore it.
  (setf (bito::first-contact-p ps1) nil)
  (push-msg q 1 :bitfield #*1100110011)
  ;; This one is only sending pieces we have already. Not interested.
  (bito::mark-piece client 0)
  (push-msg q 2 :bitfield #*1000000000)
  ;; We're interested in this one.
  (push-msg q 3 :bitfield #*0111000000)

  (bito::process-messages client)

  (is-false (bito::interested ps1))
  (is-false (bito::interested ps2))
  (is-true (bito::interested ps3))
  (is (equalp #*0000000000 (bito::piecemap ps1)))
  (is (equalp #*1000000000 (bito::piecemap ps2)))
  (is (equalp #*0111000000 (bito::piecemap ps3)))
  (is (= 0 (bito::num-desired-pieces ps1)))
  (is (= 0 (bito::num-desired-pieces ps2)))
  (is (= 3 (bito::num-desired-pieces ps3)))
  (verify-pending-messages
   client
   (list (make-instance 'bito::pending-peer-message
                        :target ps3
                        :message-id :interested
                        :data nil))))

(def-message-test handles-requests
  (bito::mark-piece client 0)
  (bito::mark-piece client 9)
  ;; They're choked, ignore.
  (push-msg q 1 :request '(:index 0 :begin 0 :length 40))
  ;; Not choked, reply.
  (setf (bito::choked ps2) nil)
  (push-msg q 2 :request '(:index 0 :begin 0 :length 40))
  ;; Request too big, ignore.
  (push-msg q 2 :request '(:index 0 :begin 0 :length 17000))
  ;; We don't have this piece, ignore.
  (push-msg q 2 :request '(:index 1 :begin 0 :length 40))
  ;; Piece doesn't exist.
  (push-msg q 2 :request '(:index 50 :begin 0 :length 40))
  ;; Various other nonsensical requests.
  (push-msg q 2 :request '(:index 0 :begin -1 :length 40))
  (push-msg q 2 :request '(:index 9 :begin 90 :length 40))
  (push-msg q 2 :request '(:index 0 :begin 130 :length 10))
  (push-msg q 2 :request '(:index 0 :begin 130 :length 0))

  (bito::process-messages client)

  (is (= 1 (length (bito::requests-list client))))
  (is (bito::piece-request-eq
       (bito::make-piece-request ps2 '(:index 0 :begin 0 :length 40))
       (first (bito::requests-list client)))))

(def-message-test handles-cancels
  (push (bito::make-piece-request ps1 '(:index 0 :begin 0 :length 40))
        (bito::requests-list client))
  (push-msg q 1 :cancel '(:index 0 :begin 0 :length 40))

  (bito::process-messages client)

  (is-false (bito::requests-list client)))

(def-message-test handles-unrequested-piece
  (push (make-instance 'bito::outstanding-request
                       :peer-index 1
                       :piece-index 0
                       :block-begin-index 0
                       :len 100
                       :request-time *curr-time*)
        (bito::outstanding-requests client))
  (push-msg q 1 :piece '(:index 0
                         :begin 10
                         :block #(1 2 3 4 5)))

  (with-dynamic-stubs ((bito::store-block nil))
    (bito::process-messages client)

    (is (= 0 (call-times-for 'bito::store-block)))
    (is (= 0 (bito::downloaded-bytes-sum ps1)))
    (is (= 1 (length (bito::outstanding-requests client))))))

(def-client-test handles-requested-pieces
  (let* ((piece-path "/tmp/piece")
         (data (make-array 100 :element-type '(unsigned-byte)
                               :initial-contents
                               (loop for i from 0 below 100
                                     collect i)))
         (torrent
           (make-instance 'torrent
                          :metainfo nil
                          :tracker-list nil
                          :info-hash nil
                          :dirname nil
                          :files (list (make-filespec "/tmp/piece" 100))
                          :total-length 100
                          :piece-length 100
                          :piece-hashes (list (bito::compute-sha1 data))
                          :num-pieces 1
                          :max-block-size 50))
         (ps1 (make-peer-state torrent 0 1))
         (client (make-test-client torrent (list ps1)))
         (q (bito::control-queue client)))
    (bito::mark-piece ps1 0)
    (setf (bito::interested ps1) t)
    (setf (bito::num-desired-pieces ps1) 1)
    (push (make-instance 'bito::outstanding-request
                         :peer-index 1
                         :piece-index 0
                         :block-begin-index 0
                         :len 50
                         :request-time 0)
          (bito::outstanding-requests client))
    (push (make-instance 'bito::outstanding-request
                         :peer-index 1
                         :piece-index 0
                         :block-begin-index 50
                         :len 50
                         :request-time 0)
          (bito::outstanding-requests client))

    (push-msg q 1 :piece (list
                          :index 0
                          :begin 50
                          :block (subseq data 50 100)))
    (bito::process-messages client)

    (is (= 50 (bito::downloaded-bytes-sum ps1)))
    (is (= 1 (length (bito::outstanding-requests client))))
    (is (= 1 (hash-table-count (bito::partial-pieces client))))
    (is (= 0 (bito::pieces-downloaded client)))
    (is-false (bito::has-piece-p client 0))
    (is-false (bito::download-complete-p client))
    (is-true (bito::interested ps1))

    (push-msg q 1 :piece (list
                          :index 0
                          :begin 0
                          :block (subseq data 0 50)))
    (bito::process-messages client)

    (is (= 100 (bito::downloaded-bytes-sum ps1)))
    (is-false (bito::interested ps1))
    (is (= 0 (bito::num-desired-pieces ps1)))
    (is (= 0 (length (bito::outstanding-requests client))))
    (is (= 0 (hash-table-count (bito::partial-pieces client))))
    (is (= 1 (bito::pieces-downloaded client)))
    (is-true (bito::has-piece-p client 0))
    (is-true (bito::download-complete-p client))
    (verify-pending-messages
     client
     (list
      (make-instance 'bito::pending-peer-message
                     :target ps1
                     :message-id :not-interested
                     :data nil)
      (make-instance 'bito::pending-peer-message
                     :target :all
                     :message-id :have
                     :data 0)))
    (let ((exists (uiop:file-exists-p piece-path))
          (buffer (make-array 100 :element-type '(unsigned-byte 8))))
      (is-true exists)
      (when exists
        (with-open-file (f piece-path :element-type '(unsigned-byte 8))
          (let ((correct-length-p (= 100 (file-length f))))
            (is-true correct-length-p)
            (when correct-length-p
              (read-sequence buffer f)
              (is (equalp data buffer))))
          (uiop:delete-file-if-exists piece-path))))))
