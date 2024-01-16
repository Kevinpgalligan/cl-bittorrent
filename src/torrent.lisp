;;;; Loading and manipulating .torrent files.

(in-package bittorrent)

(defclass torrent ()
  ((metainfo :initarg :metainfo :accessor metainfo)
   (info-hash :initarg :info-hash :accessor info-hash)
   (total-length :initarg :total-length :accessor total-length)))

(defun torrent-tracker-list (torrent)
  (with-slots (metainfo) torrent
    ;; 'announce-list' is optional, use 'announce' as a backup.
    (or (car (bencode:dict-get metainfo "announce-list"))
        (list (bencode:dict-get metainfo "announce")))))

(defun load-torrent-file (path)
  "Loads and parses a bencoded .torrent file."
  (let ((metainfo (bencode:bdecode (read-torrent-to-string path))))
    (make-instance 'torrent
                   :metainfo metainfo
                   :info-hash (compute-info-hash metainfo)
                   :total-length (compute-total-length metainfo))))

(defun read-torrent-to-string (path)
  "Reads a bencoded .torrent file into a string."
  ;; Use ISO-8859-1 because it maps each byte value to a character, so
  ;; the mix of ASCII and raw bytes in a .torrent file won't cause an error.
  (uiop:read-file-string path :external-format :iso-8859-1))

(defun compute-info-hash (metainfo &key (output-type :bytes))
  "Extracts the 'info' field from a metainfo dict and computes its SHA1
hash, which is returned as a string.
OUTPUT-TYPE determines whether the output is a string of raw bytes (:bytes) or
a string of hex digits."
  ;; Inefficient to re-encode it.
  (let ((bencoded (bencode:bencode (bencode:dict-get metainfo "info"))))
    (case output-type
      (:bytes (coerce (mapcar #'code-char (sha1:sha1-digest bencoded)) 'string))
      (:hex (string-downcase (sha1:sha1-hex bencoded)))
      (t (error "Unknown encoding type.")))))

(defun compute-total-length (metainfo)
  "Computes the total length (in bytes) of the data corresponding to the
given metainfo dict."
  (let ((info (or (bencode:dict-get metainfo "info")
                  (error "Missing info."))))
    (or (bencode:dict-get info "length")
        ;; Multi-file mode!
        (loop for file in (or (bencode:dict-get info "files")
                              (error "Missing files list."))
              sum (or (bencode:dict-get file "length")
                      (error "Missing file length."))))))
