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

(defun download-torrent (torrent-path download-path)
  (let* ((torrent (load-torrent-file torrent-path))
         (base-path (uiop:merge-pathnames* (dirname torrent) download-path))
         (piecemap (make-array (num-pieces torrent) :element-type 'bit))
         (tracker-url (random-http-tracker torrent)))
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
   (peer-states :initarg :peer-states :reader peer-states)
   (index->peer-state :initarg :index->peer-state :reader index->peer-state)
   (next-peer-index :initarg :next-peer-index :accessor next-peer-index)
   (control-queue :initarg :control-queue :reader control-queue)
   (last-update-time :initarg :last-update-time :accessor last-update-time)
   (last-tracker-ping :initarg :last-tracker-ping :accessor last-tracker-ping)
   (optimistic-unchoke :initarg :optimistic-unchoke :accessor optimistic-unchoke)
   (optimistic-unchoke-count :initarg :optimistic-unchoke-count
                             :accessor optimistic-unchoke-count)
   (downloaded-bytes :initarg :downloaded-bytes :reader downloaded-bytes)
   (uploaded-bytes :initarg :uploaded-bytes :reader uploaded-bytes)))

(defun make-client (id torrent port base-path tracker-url
                    piecemap listen-sock tracker-interval peers)
  (let* ((time-now (get-time-now))
         (peer-states (loop for peer in peers
                            for index = 1 then (1+ index)
                            collect (make-peer-state torrent time-now index)))
         (index->peer-states (make-hash-table)))
    (loop for ps in peer-states
          do (setf (gethash (index ps) index->peer-states) ps))
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
     :index->peer-states index->peer-states
     :next-index (1+ (length peers))
     :control-queue (make-queue)
     :last-update-time 0
     :last-tracker-ping time-now
     :optimistic-unchoke nil
     :optimistic-unchoke-count 0
     :downloaded-bytes 0
     :uploaded-bytes 0)))

(defun make-download-state ()
  (make-instance 'download-state :uploaded 0 :downloaded 0))

(defun get-peer-state (client index)
  (gethash index (index->peer-states client)))

(defun get-time-now ()
  (get-internal-real-time))

(defclass peer-state ()
  ((choked :initarg :choked :accessor choked)
   (they-choking :initarg :they-choking :accessor they-choking)
   (interested :initarg :interested :accessor interested)
   (they-interested :initarg :they-interested :accessor they-interested)
   (piecemap :initarg :piecemap :accessor piecemap)
   (num-desired-pieces :initarg :num-desired-pieces :accessor num-desired-pieces)
   (last-contact-time :initarg :last-contact-time :accessor last-contact-time)
   (uploaded-bytes :initarg :uploaded-bytes :accessor uploaded-bytes)
   (downloaded-bytes :initarg :downloaded-bytes :accessor downloaded-bytes)
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
                 :last-contact-time time-now
                 :uploaded-bytes (loop repeat *rolling-transfer-window*
                                       collect 0)
                 :downloaded-bytes (loop repeat *rolling-transfer-window*
                                         collect 0)
                 :downloaded-bytes-sum 0
                 :queue (make-queue)))

(defun client-loop (client)
  (handler-case
      (progn
        (spin-up-peer-threads client)
        (loop do (prune-idle-peers client)
              do (maybe-update-chokes client)
              do (maybe-ping-tracker client)
              do (process-messages client)
              do (send-piece-requests client)
              do (connect-to-new-peers client)))
    (condition (c)
      ;; TODO allow user to specify action.
      (shut-down-peer-threads client)
      (signal c))))

(defun spin-up-peer-threads (client)
  (with-accessors (peers peer-states torrent id control-queue)
      client
    (loop for peer in peers 
          for ps in peer-states 
          do (spin-up-peer-thread torrent id peer ps control-queue :peer peer))))

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
          (< (- (get-time-now) (last-contact-time ps))
             *max-idle-time*))
        (peer-states client))))

(defun kill-peer (ps)
  "Sends a message to the peer thread to shut down, and removes all
state in the client associated with that peer."
  (shut-down-peer-thread ps)
  (remove-peer-state client (index ps)))

(defun shut-down-peer-threads (client)
  (map nil #'shut-down-peer-thread (peer-states client)))

(defun shut-down-peer-thread (peer-state)
  (qpush (queue peer-state) (queue-message :tag :shutdown)))

(defun maybe-update-chokes (client)
  (when (time-to-update? client)
    (update-chokes client)
    (incf (optimistic-unchoke-count client))))

(defun time-to-update? (client)
  (> (- (get-time-now) (last-update-time client))
     *choke-update-period*))

(defun update-chokes (client)
  (with-accessors (optimistic-unchoke-count optimistic-unchoke
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
                   (send-to-peer ps
                                 (if now-choked? :choke :unchoke)
                                 nil)))))))

(defun select-peers-to-unchoke (client)
  (with-accessors (peer-states optimistic-unchoke)
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
    (maprcar #'index best)))

(defun find-worst-uploader (list)
  (alexandria:extremum list #'< :key #'downloaded-bytes-sum))

(defun update-transfer-rates (ps)
  "Upload and download rates are tracked in a rolling window, this updates them."
  (update-transfer-rate ps 'uploaded-bytes)
  (update-transfer-rate ps 'downloaded-bytes 'downloaded-bytes-sum))

(defun update-transfer-rate (ps bytes-slot &optional bytes-sum-slot)
  (let ((last-val (car (last (slot-value bs bytes-slot)))))
    (when bytes-sum-slot
      (decf (slot-value bs bytes-sum-slot) last-val)))
  (setf (slot-value bs bytes-slot)
        (cons 0 (butlast (slot-value bs bytes-slot)))))

(defun send-to-peer (peer-state message-id &optional data)
  (qpush (queue peer-state)
         (queue-message :tag :peer-message
                        :contents (make-message :id message-id
                                                :data data))))

(defun maybe-ping-tracker (client)
  (with-accessors (tracker-interval tracker-url torrent
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

(defun process-messages (client)
  (with-accessors (control-queue)
      client
    (loop for qmsg = (qpop control-queue :timeout *control-queue-timeout*)
          while qmsg
          do (handle-ctrl-message client qmsg))))

(defun handle-ctrl-message (client qmsg)
  "Deals with messages from the peer threads, sent through
the control queue. These can just be forwards of messages from
the peer."
  (case (tag qmsg)
    (:shutdown (remove-peer-state client (id qmsg)))
    (:peer-message (handle-peer-message client (id qmsg) (contents qmsg)))))

(defun remove-peer-state (client peer-index)
  ;; Don't need to update PEERS, since that's only used to
  ;; bootstrap the client.
  (setf (peer-states client)
        (remove peer-index (peer-states client) :key #'index))
  (remhash (index ps) (index->peer-states client)))

(defun handle-peer-message (client peer-index msg)
  (with-slots (id data) msg
    (let ((ps (get-peer-state client peer-index)))
      (cond
        ((member id '(:choke :unchoke))
         (setf (they-choking ps) (eq id :choke)))
        ((member id '(:interested :not-interested))
         (setf (they-interested ps) (eq id :interested)))
        ((member id '(:have :bitfield))
         (update-piecemaps client ps data)
         (when (and (should-be-interested-p ps)
                    (not (interested ps)))
           (setf (interested ps) t)
           (send-to-peer ps :interested)))
        ((eq id :request)
         ;; TODO hmmm
         ;; if choked, drop
         ;; otherwise, dedupe
         ;;    and add to list
         ;; actual sending can occur in what is now called
         ;; SEND-PIECE-REQUESTS, maybe? That will allow some buffering.
         ;; Would also need to move the FLUSH in the peer thread until
         ;; after all queue messages have been processed.
         )
        ((eq id :piece)
         ;; TODO accumulate it / write it
         ;; when entire piece written, let peers know if uninterested.
         )
        ((eq id :cancel)
         ;; TODO maybe group this with :request
         )
        )
      (setf (first-contact-p ps) nil)
      ;; Ignoring keep-alive messages but they do still have this effect.
      (setf (last-contact-time ps) (get-time-now)))))

(defun update-piecemaps (client peer-state x)
  (if (integerp x)
      (when (has-piece-p peer-state x)
        (mark-piece peer-state x)
        (when (not (has-piece-p client))
          (incf (num-desired-pieces peer-state))))
      ;; It's a bitfield, only allowed as the first message.
      (when (first-contact-p peer-state)
        (set-piecemap client peer-state x))))

(defun has-piece-p (obj i)
  (= 1 (bit (piecemap peer-state) i)))

(defun mark-piece (obj i)
  (setf (bit (piecemap peer-state) i) 1))

(defun should-be-interested-p (peer-state)
  (> (num-desired-pieces peer-state) 0))

(defun set-piecemap (client peer-state bitfield)
  (bit-ior (piecemap peer-state) x t)
  (setf (num-desired-pieces peer-state)
        (count-bits (bit-andc1 (piecemap client) x))))

(defun count-bits (bv)
  (loop for b across bv sum b))

(defun send-piece-requests (client)
  ;; TODO
  ;; torrent piecemap peer-states
  ;; 1. send requests
  ;; 2. send keepalives
  ;; 3. send blocks that they requested
  )

(defun connect-to-new-peers (client)
  (with-accessors (listen-sock torrent id control-queue next-peer-index)
      client
    (when (usocket:wait-for-input listen-sock :timeout 0 :ready-only t)
      (let ((peer-state (make-peer-state torrent (get-time-now) next-peer-index)))
        (add-peer-state client)
        (spin-up-peer-thread torrent
                             id
                             peer-state
                             control-queue
                             :sock (usocket:socket-accept
                                    listen-sock
                                    :element-type '(unsigned-byte 8)))))))

(defun add-peer-state (client ps)
  (with-accessors (next-peer-index peer-states index->peer-states)
      client
    (incf next-peer-index)
    (push ps peer-states)
    (setf (gethash (index ps) index->peer-states) ps)))
