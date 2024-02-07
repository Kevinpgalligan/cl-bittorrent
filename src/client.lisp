(in-package bittorrent)

(defparameter *choke-update-period*
  (* internal-time-units-per-second 10))
(defparameter *optimistic-unchoke-period* 3
  "How often to switch optimistically unchoked peer, measured in
terms of the number of 'choke update' intervals.")
(defparameter *rolling-transfer-window* 2)
(defparameter *peers-to-unchoke* 4)
(defparameter *control-queue-timeout* 0.1)
(defparameter *max-idle-time*
  (* internal-time-units-per-second 120))
(defparameter *piece-request-timeout*
  (* internal-time-units-per-second 120))

(defparameter *max-concurrent-piece-requests* 4)

(defun download-torrent (torrent-path download-path)
  (let* ((torrent (load-torrent-file torrent-path))
         (base-path (uiop:merge-pathnames* (dirname torrent) download-path))
         (piecemap (make-array (num-pieces torrent) :element-type 'bit))
         (tracker-url (random-http-tracker torrent)))
    (loop for filespec in (files torrent)
          do (add-base-path base-path filespec))
    (multiple-value-bind (listen-sock port) (attempt-open-bittorrent-socket)
      (multiple-value-bind (tracker-response peer-id)
          (query-tracker torrent
                         :port port
                         :tracker-url tracker-url
                         :event :started)
        (when (warning-message tracker-response)
          (format t "Tracker warning: ~a~%" (warning-message tracker-response)))
        (cond
          ((failure-reason tracker-response)
           (format t "Failed to contact tracker: ~a~%"
                   (failure-reason tracker-response)))
          (t
           (client-loop
            (make-client peer-id torrent port base-path tracker-url
                         piecemap listen-sock
                         (interval tracker-response)
                         (peers tracker-response)))))))))

(defun attempt-open-bittorrent-socket ()
  (or
   (loop for port from 6881 below 6890
         for sock = (handler-case (usocket:socket-listen
                                   "127.0.0.1" port :element-type 'flexi-streams:octet)
                      (usocket:address-in-use-error ()
                        nil))
         when sock
           do (return (values sock port)))
   (error "Couldn't find a free port in the range 6881-6889.")))

(defclass client ()
  ((id :initarg :id :reader id)
   (torrent :initarg :torrent :reader torrent)
   (port :initarg :port :reader port)
   (base-path :initarg :base-path :reader base-path)
   (tracker-url :initarg :tracker-url :reader tracker-url)
   (piecemap :initarg :piecemap :reader piecemap)
   (listen-sock :initarg :listen-sock :reader listen-sock)
   (tracker-interval :initarg :tracker-interval :reader tracker-interval)
   (peers :initarg :peers :reader peers)
   (peer-states :initarg :peer-states :accessor peer-states)
   (index->peer-state :initarg :index->peer-state :reader index->peer-state)
   (next-peer-index :initarg :next-peer-index :accessor next-peer-index)
   (control-queue :initarg :control-queue :reader control-queue)
   (last-update-time :initarg :last-update-time :accessor last-update-time
                     ;; Could change the name rather than writing docs and writing
                     ;; this comment, but oh well.
                     :documentation "Last time we updated which peers to choke.")
   (last-tracker-ping :initarg :last-tracker-ping :accessor last-tracker-ping)
   (optimistic-unchoke :initarg :optimistic-unchoke :accessor optimistic-unchoke)
   (optimistic-unchoke-count :initarg :optimistic-unchoke-count
                             :accessor optimistic-unchoke-count)
   (downloaded-bytes :initarg :downloaded-bytes :accessor downloaded-bytes)
   (uploaded-bytes :initarg :uploaded-bytes :accessor uploaded-bytes)
   (requests-list :initarg :requests-list :accessor requests-list)
   (pending-messages :initarg :pending-messages :accessor pending-messages)
   (partial-pieces :initarg :partial-pieces :reader partial-pieces)
   (outstanding-requests :initarg :outstanding-requests :accessor outstanding-requests)))

(defun make-client (id torrent port base-path tracker-url
                    piecemap listen-sock tracker-interval peers)
  (let* ((time-now (get-time-now))
         (peer-states (loop for peer in peers
                            for index = 1 then (1+ index)
                            collect (make-peer-state torrent time-now index)))
         (index->peer-state (make-hash-table)))
    (loop for ps in peer-states
          do (setf (gethash (index ps) index->peer-state) ps))
    (make-instance
     'client
     :id id
     :torrent torrent
     :port port
     :base-path base-path
     :tracker-url tracker-url
     :piecemap piecemap
     :listen-sock listen-sock
     :tracker-interval tracker-interval
     :peers peers
     :peer-states peer-states
     :index->peer-state index->peer-state
     :next-index (1+ (length peers))
     :control-queue (make-queue)
     :last-update-time 0
     :last-tracker-ping time-now
     :optimistic-unchoke nil
     :optimistic-unchoke-count 0
     :downloaded-bytes 0
     :uploaded-bytes 0
     :requests-list nil
     :pending-messages nil
     :partial-pieces (make-hash-table)
     :outstanding-requests nil)))

(defun make-download-state ()
  (make-instance 'download-state :uploaded 0 :downloaded 0))

(defun get-peer-state (client index)
  (gethash index (index->peer-state client)))

(defun get-time-now ()
  (get-internal-real-time))

(defclass peer-state ()
  ((choked :initarg :choked :accessor choked)
   (they-choking :initarg :they-choking :accessor they-choking)
   (interested :initarg :interested :accessor interested)
   (they-interested :initarg :they-interested :accessor they-interested)
   (piecemap :initarg :piecemap :accessor piecemap)
   (num-desired-pieces :initarg :num-desired-pieces :accessor num-desired-pieces)
   (last-receive-time :initarg :last-receive-time :accessor last-receive-time)
   (last-send-time :initarg :last-send-time :accessor last-send-time)
   (have-sent-p :initarg :have-sent-p :accessor have-sent-p)
   (uploaded-bytes :initarg :uploaded-bytes :accessor uploaded-bytes)
   (downloaded-bytes :initarg :downloaded-bytes :accessor downloaded-bytes)
   (downloaded-bytes-sum :initarg :downloaded-bytes-sum
                         :accessor downloaded-bytes-sum)
   (queue :initarg :queue :reader queue)
   (first-contact-p :initform t :accessor first-contact-p)))

(defun make-peer-state (torrent time-now index)
  (make-instance 'peer-state
                 :index index
                 :choked t
                 :they-choking t
                 :interested nil
                 :they-interested nil
                 :piecemap (make-array (num-pieces torrent) :element-type 'bit)
                 :num-desired-pieces 0
                 :last-receive-time time-now
                 :last-send-time time-now
                 :have-sent-p nil
                 :uploaded-bytes (loop repeat *rolling-transfer-window*
                                       collect 0)
                 :downloaded-bytes (loop repeat *rolling-transfer-window*
                                         collect 0)
                 :downloaded-bytes-sum 0
                 :queue (make-queue)))

(defun client-loop (client)
  (spin-up-peer-threads client)
  (loop do (prune-idle-peers client)
        do (prune-timed-out-piece-requests client)
        do (maybe-update-chokes client)
        do (maybe-ping-tracker client)
        do (process-messages client)
        do (send-peer-messages client)
        do (connect-to-new-peers client)))

(defun spin-up-peer-threads (client)
  (with-slots (peers peer-states torrent id control-queue)
      client
    (loop for peer in peers 
          for ps in peer-states 
          do (spin-up-peer-thread torrent id ps control-queue :peer peer))))

(defun spin-up-peer-thread (torrent id peer-state control-queue &key peer sock)
  (bt:make-thread
   (lambda ()
     (peer-loop torrent id (queue peer-state) control-queue
                (index peer-state) :peer peer :sock sock))))

(defun prune-idle-peers (client)
  (map nil
       (lambda (ps)
         (kill-peer client ps))
       (remove-if
        (lambda (ps)
          (< (- (get-time-now) (last-receive-time ps))
             *max-idle-time*))
        (peer-states client))))

(defun kill-peer (client ps)
  "Sends a message to the peer thread to shut down, and removes all
state in the client associated with that peer."
  (shut-down-peer-thread ps)
  (remove-peer-state client (index ps)))

(defun shut-down-peer-threads (client)
  (map nil #'shut-down-peer-thread (peer-states client)))

(defun shut-down-peer-thread (peer-state)
  (qpush (queue peer-state) (queue-message :tag :shutdown)))

(defun prune-timed-out-piece-requests (client)
  (let ((now (get-time-now)))
    (setf (outstanding-requests client)
          (remove-if (lambda (req)
                       (> (- now (request-time req))
                          *piece-request-timeout*))
                     (outstanding-requests client)))))

(defun maybe-update-chokes (client)
  (when (time-to-update? client)
    (update-chokes client)
    (incf (optimistic-unchoke-count client))))

(defun time-to-update? (client)
  (> (- (get-time-now) (last-update-time client))
     *choke-update-period*))

(defun update-chokes (client)
  (with-slots (optimistic-unchoke-count optimistic-unchoke
               peer-states)
      client
    (when (zerop optimistic-unchoke-count)
      ;; Time to update which peer we're optimistically unchoking.
      (setf optimistic-unchoke 
            (index (alexandria:random-elt peer-states))))
    (let ((next-unchoked (select-peers-to-unchoke client)))
      (loop for ps in peer-states
            do (update-transfer-rates ps)
            do (let* ((now-choked? (not (member (index ps) next-unchoked)))
                      (changed? (not (eq now-choked? (choked ps)))))
                 (setf (choked ps) now-choked?)
                 (when changed?
                   ;; Let the peer know their new status.
                   (prepare-peer-message client
                                         ps
                                         (if now-choked? :choke :unchoke))))))))

(defun select-peers-to-unchoke (client)
  (with-slots (peer-states optimistic-unchoke)
      client
    (let* ((most-generous (get-best-uploaders peer-states optimistic-unchoke)))
      (cons optimistic-unchoke most-generous))))

(defun get-best-uploaders (peer-states optimistic-unchoke)
  (let ((best nil)
        (worst-download-amount 0))
    (loop for ps in peer-states
          ;; Exclude the optimistic unchoke from the selection.
          when (and (not (= (index ps) optimistic-unchoke))
                    (or (< (length best) *peers-to-unchoke*)
                        (> (downloaded-bytes-sum ps) worst-download-amount)))
            do (progn
                 (push ps best)
                 (when (> (length best) *peers-to-unchoke*)
                   (let ((worst-index (index (find-worst-uploader best))))
                     (setf best (remove-if (lambda (i) (= i worst-index))
                                           best
                                           :key #'index))))
                 (setf worst-download-amount
                       (downloaded-bytes-sum (find-worst-uploader best)))))
    (mapcar #'index best)))

(defun find-worst-uploader (list)
  (alexandria:extremum list #'< :key #'downloaded-bytes-sum))

(defun update-transfer-rates (ps)
  "Upload and download rates are tracked in a rolling window, this updates them."
  (update-transfer-rate ps 'uploaded-bytes)
  (update-transfer-rate ps 'downloaded-bytes 'downloaded-bytes-sum))

(defun update-transfer-rate (ps bytes-slot &optional bytes-sum-slot)
  (let ((last-val (car (last (slot-value :s bytes-slot)))))
    (when bytes-sum-slot
      (decf (slot-value ps bytes-sum-slot) last-val)))
  (setf (slot-value ps bytes-slot)
        (cons 0 (butlast (slot-value ps bytes-slot)))))

(defun send-to-peer (peer-state message-id &optional data)
  (qpush (queue peer-state)
         (queue-message :tag :peer-message
                        :contents (make-message :id message-id
                                                :data data)))
  (setf (last-send-time peer-state) (get-time-now))
  (setf (have-sent-p peer-state) t))

(defclass pending-peer-message ()
  ((target :initarg :target :reader target
           :documentation "Can be a peer-state or the keyword symbol :ALL.")
   (message-id :initarg :message-id :reader message-id)
   (data :initarg :data :reader data)))

(defun prepare-peer-message (client target message-id &optional data)
  (push (make-instance 'pending-peer-message
                       :target target
                       :message-id message-id
                       :data data)
        (pending-messages client)))

(defun maybe-ping-tracker (client)
  (with-slots (tracker-interval tracker-url torrent
               port last-tracker-ping downloaded-bytes uploaded-bytes)
      client
    (when (> (elapsed-time last-tracker-ping) tracker-interval)
      ;; Not currently using the tracker response for anything, let new
      ;; peers be the ones to connect to us.
      (query-tracker torrent
                     :port port
                     :tracker-url tracker-url
                     :bytes-left (- (total-length torrent) downloaded-bytes)
                     :uploaded uploaded-bytes))))

(defun elapsed-time (timestamp)
  (- (get-time-now) timestamp))

(defun process-messages (client)
  (with-slots (control-queue)
      client
    (loop for qmsg = (qpop control-queue *control-queue-timeout*)
          while qmsg
          do (handle-ctrl-message client qmsg))))

(defun handle-ctrl-message (client qmsg)
  "Deals with messages from the peer threads, sent through
the control queue. These can just be forwards of messages from
the peer."
  (case (tag qmsg)
    (:shutdown
     (remove-peer-state client (id qmsg))
     (remove-outstanding-requests-for-peer client (id qmsg)))
    (:peer-message (handle-peer-message client (id qmsg) (contents qmsg)))))

(defun remove-peer-state (client peer-index)
  ;; Don't need to update PEERS, since that's only used to
  ;; bootstrap the client.
  (setf (peer-states client)
        (remove peer-index (peer-states client) :key #'index))
  (remhash peer-index (index->peer-state client)))

(defun remove-outstanding-requests-for-peer (client peer-index)
  (setf (outstanding-requests client)
        (remove-if (lambda (req)
                     (= peer-index (peer-index req)))
                   (outstanding-requests client))))

(defun find-outstanding-block-request (client peer-index piece-index
                                       block-begin-index len)
  (loop for req in (outstanding-requests client)
        when (and (= peer-index (peer-index req))
                  (= piece-index (piece-index req))
                  (= block-begin-index (block-begin-index req))
                  (= len (len req)))
          do (return req)))

(defun handle-peer-message (client peer-index msg)
  ;; Some comments to help my future self. DATA here is basically the payload
  ;; of a message from a peer. Right now it's a plist, the fields of which depend
  ;; on the message type but should hopefully have obvious names from the
  ;; standard / should be gleanable from the message-parsing. Here, we don't
  ;; send any messages to peers, we just queue up messages for later.
  (with-slots (id data) msg
    (let ((ps (get-peer-state client peer-index)))
      (cond
        ((member id '(:choke :unchoke))
         (setf (they-choking ps) (eq id :choke))
         (when (they-choking ps)
           (remove-outstanding-requests-for-peer client (index ps))))
        ((member id '(:interested :not-interested))
         (setf (they-interested ps) (eq id :interested)))
        ((member id '(:have :bitfield))
         (update-piecemaps client ps data)
         (when (and (should-be-interested-p ps)
                    (not (interested ps)))
           (setf (interested ps) t)
           (prepare-peer-message client ps :interested)))
        ((eq id :request)
         ;; If they're choked we just drop the request.
         (when (and (not (choked ps))
                    (has-piece-p client (getf data :index))
                    (not (> (getf data :length) *max-request-size*)))
           (add-to-requests-list client ps data)))
        ((eq id :piece)
         ;; If we didn't request it, ignore.
         (alexandria:when-let
             ((req (find-outstanding-block-request
                    client
                    (index ps)
                    (getf data :index)
                    (getf data :begin)
                    (getf data :length))))
           (setf (outstanding-requests client)
                 (remove req (outstanding-requests client)))
           (store-block client data)
           (let ((piece-index (getf data :index)))
             (when (has-piece-p client piece-index)
               ;; Let everyone know we have this piece now.
               (prepare-peer-message client :all :have piece-index)
               ;; Update whether we're interested in peers now.
               (map nil
                    (lambda (ps)
                      (when (has-piece-p ps piece-index)
                        (decf (num-desired-pieces ps))
                        (when (zerop (num-desired-pieces ps))
                          (setf (interested ps) nil)
                          (prepare-peer-message client ps :not-interested))))
                    (peer-states client))))))
        ((eq id :cancel)
         (setf (requests-list client)
               (remove (make-piece-request ps data)
                       (requests-list client)
                       :test #'piece-request-eq))))
      (setf (first-contact-p ps) nil)
      ;; Ignoring keep-alive messages but they do still have this effect.
      (setf (last-receive-time ps) (get-time-now)))))

(defun update-piecemaps (client peer-state x)
  (if (integerp x)
      (when (has-piece-p peer-state x)
        (mark-piece peer-state x)
        (when (not (has-piece-p client x))
          (incf (num-desired-pieces peer-state))))
      ;; It's a bitfield, only allowed as the first message.
      (when (first-contact-p peer-state)
        (set-piecemap peer-state x))))

(defun has-piece-p (obj i)
  (= 1 (bit (piecemap obj) i)))

(defun mark-piece (obj i)
  (setf (bit (piecemap obj) i) 1))

(defun should-be-interested-p (peer-state)
  (> (num-desired-pieces peer-state) 0))

(defun set-piecemap (peer-state bitfield)
  (bit-ior (piecemap peer-state) bitfield t)
  (setf (num-desired-pieces peer-state)
        (count-bits (piecemap peer-state))))

(defun count-bits (bv)
  (loop for b across bv sum b))

(defclass piece-request ()
  ((peer-state :initarg :peer-state :reader peer-state)
   (index :initarg :index :reader index)
   (begin :initarg :begin :reader begin)
   (len :initarg :len :reader len)))

(defun piece-request-eq (req1 req2)
  (and
   ;; The peer index is different from the piece index. Bad
   ;; naming, perhaps.
   (= (index (peer-state req1)) (index (peer-state req2)))
   (= (index req1) (index req2))
   (= (begin req1) (begin req2))
   (= (len req1) (len req2))))

(defun make-piece-request (ps request-data)
  (make-instance 'piece-request
                 :peer-state ps
                 :index (getf request-data :index)
                 :begin (getf request-data :begin)
                 :len (getf request-data :length)))

(defun add-to-requests-list (client ps request-data)
  (let ((req (make-piece-request ps request-data)))
    (when (not (in-requests-list-p client req))
      (push req (requests-list client)))))

(defun in-requests-list-p (client req)
  (member req (requests-list client) :test #'piece-request-eq))

(defun store-block (client block-data)
  (with-slots (torrent) client
    (let* ((b (make-block (getf block-data :index)
                          (getf block-data :begin)
                          (getf block-data :block)))
           (partial-piece (gethash (index b) (partial-pieces client)))
           (piece-start (* (index b) (piece-length torrent))))
      (when (null partial-piece)
        (setf partial-piece
              (make-partial-piece (index b)
                                  piece-start
                                  (min (+ piece-start (piece-length torrent))
                                       (total-length torrent))))
        (setf (gethash (index b) (partial-pieces client)) partial-piece))
      (block-insert partial-piece b)
      (pop-and-write-piece-if-ready client partial-piece))))

(defun pop-and-write-piece-if-ready (client partial-piece)
  (when (piece-ready-p partial-piece)
    (remhash (piece-index partial-piece) (partial-pieces client))
    (let ((piece (stitch-together-piece partial-piece))
          (torrent (torrent client)))
      (when (valid-piece-p (nth (index piece) (piece-hashes torrent)) piece)
        (write-piece piece (files torrent))
        (incf (downloaded-bytes client) (size piece))
        (mark-piece client (index piece))))))

(defun send-peer-messages (client)
  ;; Putting this first allows us to send the bitfield as our first
  ;; message, which is the only time it's allowed.
  (send-proactive-messages client)
  (send-pending-messages client)
  (send-requested-blocks client)
  (send-keepalives client))

(defun send-pending-messages (client)
  (loop for msg in (pending-messages client)
        do (flet ((send (peer-state)
                    ;; I think it's okay to not copy the data, since the
                    ;; peer threads shouldn't mutate it.
                    (send-to-peer peer-state (message-id msg) (data msg))))
             (if (eq :all (target msg))
                 ;; Send to all peers!
                 (map nil #'send (peer-states client))
                 ;; It's a peer-state, just send to this peer.
                 (send (target msg)))))
  (setf (pending-messages client) nil))

(defun send-requested-blocks (client)
  (loop for req in (requests-list client)
        do (send-requested-block client req)))

(defun send-requested-block (client req)
  (with-slots (torrent) client
    (let ((b (load-bytes-from-files
              (torrent client)
              (begin req)
              (+ (begin req) (len req)))))
      (incf (uploaded-bytes client) (length b))
      (send-to-peer (peer-state req)
                    :piece
                    (list :index (index req)
                          :begin (begin req)
                          :block b)))))

(defun send-proactive-messages (client)
  "Messages that aren't responses to messages from peers."
  ;; We can only send the bitfield as our first message to
  ;; a peer, but no point sending it if we don't have any pieces.
  (when (not (zerop (downloaded-bytes client)))
    (loop for ps in (peer-states client)
          when (not (have-sent-p ps))
            do (send-to-peer ps :bitfield (piecemap client))))
  ;; Now request pieces!
  (with-slots (torrent) client
    (let ((available-peers (get-available-peers client)))
      (labels ((get-peers-with-piece (piece-index)
                 (remove-if (lambda (ps)
                              (not (has-piece-p ps piece-index)))
                            available-peers))
               (remove-peer-state (list index)
                 (remove index list :key #'index))
               (remove-available! (ps)
                 (setf available-peers
                       (remove-peer-state available-peers (index ps)))))
        ;; Prioritise the pieces that are already in progress, since having
        ;; full pieces lets us share them with others and get unchoked.
        (loop for partial-piece being the hash-values of (partial-pieces client)
                using (hash-key piece-index)
              while available-peers
              for peers-with-piece = (get-peers-with-piece (index partial-piece))
              ;; Check for outstanding requests. Find the next block
              ;; for this piece that doesn't have an outstanding request. Then
              ;; request it from a random peer that has the piece.
              do (loop while peers-with-piece
                       for b in (remaining-blocks client partial-piece)
                       ;; We have available peers with this piece - request data!
                       ;; Ugly duplication that I'm not sure how to get rid of.
                       ;; A function doesn't work 'cause it can't setf
                       ;; peers-with-pieces.
                       do (let ((ps (alexandria:random-elt peers-with-piece)))
                            (send-piece-request client ps b)
                            (when (max-capacity-p ps)
                              (remove-available! ps)
                              (setf peers-with-piece
                                    (remove-peer-state peers-with-piece
                                                       (index ps)))))))
        ;; If there are still available peers, send them requests
        ;; for new pieces, if possible.
        (loop for i upto (num-pieces torrent)
              while available-peers
              when (and (not (has-piece-p client i))
                        (null (gethash i (partial-pieces client))))
                do (loop with peers-with-piece = (get-peers-with-piece i)
                         while peers-with-piece
                         for b in (remove-if
                                   (lambda (b)
                                     (request-outstanding-p
                                      client i (getf b :begin)))
                                   (all-blocks torrent i))
                         ;; Ugly duplication.
                         do (let ((ps (alexandria:random-elt peers-with-piece)))
                              (send-piece-request client ps b)
                              (when (max-capacity-p ps)
                                (remove-available! ps)
                                (setf peers-with-piece
                                      (remove-peer-state peers-with-piece
                                                         (index ps)))))))))))

(defun get-available-peers (client)
  (remove-if (lambda (ps)
               (or (they-choking ps)
                   (max-capacity-p ps)))
             (peer-states client)))

(defun max-capacity-p (ps)
  (> (length (outstanding-requests ps)) *max-concurrent-piece-requests*))

(defclass outstanding-request ()
  ((peer-index :initarg :peer-index :reader peer-index)
   (piece-index :initarg :piece-index :reader piece-index)
   (block-begin-index :initarg :block-begin-index
                      :reader block-begin-index)
   (len :initarg :len :reader len)
   (request-time :initarg :request-time :reader request-time)))

(defun send-piece-request (client ps b)
  (send-to-peer ps
                :request
                (list :index (getf b :piece-index)
                      :begin (getf b :begin)
                      :length (getf b :length)))
  (push (make-instance 'outstanding-request
                       :peer-index (index ps)
                       :piece-index (getf b :piece-index)
                       :block-begin-index (getf b :begin)
                       :length (getf b :length)
                       :request-time (get-time-now))
        (outstanding-requests client)))

(defun remaining-blocks (client partial-piece)
  (flet ((accumulated-or-outstanding-p (b)
           (or
            (find (getf b :begin) (blocks partial-piece) :key #'start)
            (request-outstanding-p client
                                   (piece-index partial-piece)
                                   (getf b :begin)))))
    (remove-if #'accumulated-or-outstanding-p
               (all-blocks (torrent client) (piece-index partial-piece)))))

(defun request-outstanding-p (client piece-index block-begin-index)
  (loop for req in (outstanding-requests client)
          thereis (and (= piece-index (piece-index req))
                       (= block-begin-index (block-begin-index req)))))

(defun send-keepalives (client)
  (let ((now (get-time-now)))
    (loop for ps in (peer-states client)
          when (> (- now (last-send-time ps)) *max-idle-time*)
            do (send-to-peer ps :keep-alive))))

(defun connect-to-new-peers (client)
  (with-slots (listen-sock torrent id control-queue next-peer-index)
      client
    (when (usocket:wait-for-input listen-sock :timeout 0 :ready-only t)
      (let ((peer-state (make-peer-state torrent (get-time-now) next-peer-index)))
        (add-peer-state client peer-state)
        (spin-up-peer-thread torrent
                             id
                             peer-state
                             control-queue
                             :sock (usocket:socket-accept
                                    listen-sock
                                    :element-type '(unsigned-byte 8)))))))

(defun add-peer-state (client ps)
  (with-slots (next-peer-index peer-states index->peer-state)
      client
    (incf next-peer-index)
    (push ps peer-states)
    (setf (gethash (index ps) index->peer-state) ps)))
