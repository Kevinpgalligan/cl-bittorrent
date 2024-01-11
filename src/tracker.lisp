;;;; Communicating with trackers over HTTP/HTTPS.

(in-package bittorrent)

(defun tracker-list (metainfo)
  ;; 'announce-list' is optional, use 'announce' as a backup.
  (or (car (bencode:dict-get metainfo "announce-list"))
      (list (bencode:dict-get metainfo "announce"))))

(defun fetch-peers (metainfo peer-url &key (port 6881) peer-id)
  (when (not peer-id)
    ;; Use a random string. Should do something smarter.
    (setf peer-id (coerce (loop repeat 20 collect (code-char (random 256)))
                          'string)))
  ;; This is necessary to get the message in a nice string form, for parsing
  ;; by esrap. dex returns a byte array because of the option :force-binary.
  ;; (This option is necessary because otherwise it treats the output as UTF-8).
  (flexi-streams:octets-to-string
   (dex:get
    (quri:render-uri
     (quri:make-uri :defaults peer-url
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
                      (cons "info_hash" (compute-info-hash metainfo))
                      (cons "peer_id" peer-id)
                      (cons "port" port)
                      (cons "uploaded" 0)
                      (cons "downloaded" 0)
                      (cons "left" (compute-total-length metainfo))
                      (cons "compact" 1)
                      ;; Need to handle "stopped" and "completed" events too.
                      (cons "event" "started"))
                     :encoding :iso-8859-1)))
    :force-binary t)))

(defclass peer ()
  ((ip :initarg :ip :accessor ip)
   (port :initarg :port :accessor port)))

(defun parse-peers-list (string)
  "Parses peer list from tracker response."
  (let* ((bytestream
           (flexi-streams:make-in-memory-input-stream
            (flexi-streams:string-to-octets
             (bencode:dict-get (bencode:bdecode string)
                               "peers")))))
    (loop while (flexi-streams:peek-byte bytestream nil nil)
          collect (make-instance
                   'peer
                   ;; Read backwards due to big-endian order.
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
