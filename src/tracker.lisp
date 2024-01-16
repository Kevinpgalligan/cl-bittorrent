;;;; Communicating with trackers.

(in-package bittorrent)

(defclass tracker-response ()
  ((failure-reason :initarg :failure-reason :accessor failure-reason)
   (warning-message :initarg :warning-message :accessor warning_message)
   (interval :initarg :interval :accessor interval)
   (min-interval :initarg :min-interval :accessor interval)
   (tracker-id :initarg :tracker-id :accessor tracker-id)
   (complete :initarg :complete :accessor complete)
   (incomplete :initarg :incomplete :accessor incomplete)
   (peers :initarg :peers :accessor incomplete)))

(defun tracker-http-p (tracker-url)
  (let ((uri (quri:make-uri :defaults tracker-url)))
    (or (quri:uri-http-p uri)
        (quri:uri-https-p uri))))

(defun query-tracker (torrent &key (port 6881) tracker-url peer-id verbose)
  "Queries a tracker over HTTP. If no TRACKER-URL (string) is supplied, one
is chosen randomly from the torrent. PORT is the port that is advertised to
the tracker. PEER-ID is the 20-byte ID of the client; if not provided, a
random one is used. If VERBOSE is true, prints stuff to standard output.
Returns the response and the peer ID."
  (when (null tracker-url)
    (let ((http-trackers (remove-if-not #'tracker-http-p
                                        (torrent-tracker-list torrent))))
      (when (null http-trackers)
        (error "No HTTP trackers for this torrent!"))
      (setf tracker-url (alexandria:random-elt http-trackers))
      (when verbose
        (format t "Using tracker: ~a~%" tracker-url))))
  (when (not peer-id)
    (setf peer-id (coerce (loop repeat 20 collect (code-char (random 256)))
                          'string)))
  (let ((bendata (send-http-request-to-tracker tracker-url torrent peer-id port
                                               :verbose verbose)))
    (values
     (make-instance 'tracker-response
                    :failure-reason (bencode:dict-get bendata "failure reason")
                    :warning-message (bencode:dict-get bendata "warning message")
                    :interval (bencode:dict-get bendata "interval")
                    :min-interval (bencode:dict-get bendata "min interval")
                    :tracker-id (bencode:dict-get bendata "tracker id")
                    :complete (bencode:dict-get bendata "complete")
                    :incomplete (bencode:dict-get bendata "incomplete")
                    :peers (parse-peers bendata))
     peer-id)))

(defun send-http-request-to-tracker (url torrent peer-id port &key verbose)
  (let ((uri (make-tracker-uri url torrent peer-id port)))
    (when verbose
      (format t "Requesting tracker: ~a~%" uri))
    ;; Must provide dex with the ':force-binary' option, as otherwise
    ;; it will mess up the response with UTF-8 encoding shenanigans.
    ;; This returns an array of bytes ("octets"), so we then convert back
    ;; to a string for parsing.
    (bencode:bdecode
     (flexi-streams:octets-to-string
      (dex:get uri :force-binary t)))))

(defun make-tracker-uri (url torrent peer-id port)
  ;; Rendering because I was getting 400 responses
  ;; when I used the URI object.
  (quri:render-uri
   (quri:make-uri
    :defaults url
    ;; Other (optional) keys:
    ;;   no_peer_id, ip, numwant, key, trackerid,
    :scheme "http"
    :path "announce"
    :query
    ;; Have to encode before passing, because otherwise
    ;; it uses the default encoding (UTF-8) which will mess
    ;; up the bytestring parameters like info_hash.
    (quri:url-encode-params
     (list
      (cons "info_hash" (info-hash torrent))
      (cons "peer_id" peer-id)
      (cons "port" port)
      (cons "uploaded" 0)
      (cons "downloaded" 0)
      (cons "left" (total-length torrent))
      (cons "compact" 1)
      ;; Need to handle "stopped" and "completed" events too.
      (cons "event" "started"))
     :encoding :iso-8859-1))))

(defclass peer ()
  ((ip :initarg :ip :accessor ip)
   (port :initarg :port :accessor port)
   (id :initarg :id :accessor id)))

(defun parse-peers (bendata)
  "Parses list from tracker response."
  (let ((peers (bencode:dict-get bendata "peers")))
    (if (listp peers)
        (extract-peers-from-list peers)
        (parse-peers-from-bytestring peers))))

(defun extract-peers-from-list (peers)
  (loop for peer in peers
        collect (make-instance 'peer
                               :ip (bencode:dict-get peer "ip")
                               :port (bencode:dict-get peer "port")
                               :id (bencode:dict-get peer "peer id"))))
  
(defun parse-peers-from-bytestring (string)
  (let* ((bytestream
           (flexi-streams:make-in-memory-input-stream
            (flexi-streams:string-to-octets string))))
    (loop while (flexi-streams:peek-byte bytestream nil nil)
          collect (make-instance 'peer
                                 :ip (read-ip bytestream)
                                 :port (read-port bytestream)))))

(defun read-ip (bytestream)
  (let ((ip (make-array '(4) :element-type '(unsigned-byte 8))))
    (read-sequence ip bytestream)
    (str:join "." (loop for byte across ip
                        collect (format nil "~a" byte)))))

(defun read-port (bytestream)
  ;; Big-endian order.
  (+ (* 256 (read-byte bytestream))
     (read-byte bytestream)))
