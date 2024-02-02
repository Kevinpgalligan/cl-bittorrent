;;;; Loading and manipulating .torrent files.

(in-package bittorrent)

(defclass torrent ()
  ((metainfo :initarg :metainfo :accessor metainfo)
   (tracker-list :initarg :tracker-list :accessor tracker-list)
   (info-hash :initarg :info-hash :accessor info-hash)
   (dirname :initarg :dirname :reader dirname)
   (files :initarg :files :reader files)
   (total-length :initarg :total-length :accessor total-length)
   (piece-length :initarg :piece-length :reader piece-length)
   (piece-hashes :initarg :piece-hashes :reader piece-hashes)))

(defmethod num-pieces ((instance torrent))
  (length (piece-hashes instance)))

(defclass filespec ()
  ((name :initarg :name :reader name)
   (len :initarg :len :reader len)
   (path :initarg :path :reader path)))

(defun make-filespec (name len path)
  (make-instance 'filespec :name name :len len :path path))

(defun extract-tracker-list (metainfo)
  (if (bencode:dict-has metainfo "announce-list")
      (first (bencode:dict-get metainfo "announce-list"))
      (list (bencode:dict-get metainfo "announce"))))

(defun load-torrent-file (path)
  "Loads and parses a bencoded .torrent file."
  (let* ((metainfo (bencode:bdecode (read-torrent-to-string path)))
         (info (bencode:dict-get metainfo "info"))
         (files (extract-files metainfo info)))
    (make-instance 'torrent
                   :metainfo metainfo
                   :tracker-list (extract-tracker-list metainfo)
                   :info-hash (compute-info-hash metainfo)
                   :dirname (and (multifile-mode-p metainfo)
                                 (bencode:dict-get info "name"))
                   :files files
                   :total-length (reduce #'+ (mapcar #'len files) :initial-value 0)
                   :piece-length (bencode:dict-get info "piece length")
                   :piece-hashes (extract-piece-hashes metainfo))))

(defun multifile-mode-p (metainfo)
  (not (bencode:dict-has metainfo "info" "length")))

(defun extract-files (metainfo info)
  (if (multifile-mode-p metainfo)
      (flet ((extract-filespec (raw)
               (make-filespec (bencode:dict-get raw "name")
                              (bencode:dict-get raw "length")
                              (bencode:dict-get raw "path"))))
        (mapcar #'extract-filespec (bencode:dict-get info "files")))
      (list (make-filespec (bencode:dict-get info "name")
                           (bencode:dict-get info "length")
                           ;; No path provided for a single file.
                           nil))))

(defun extract-piece-hashes (metainfo)
  (let ((raw-hashes (bencode:dict-get metainfo "info" "pieces")))
    (if (not (= 0 (mod (length raw-hashes) 20)))
        (error "'pieces' field of info dict is not a multiple of 20.")
        (loop for i = 0 then (+ i 20)
              while (< i (length raw-hashes))
              collect (subseq raw-hashes i (+ i 20))))))

(defun read-torrent-to-string (path)
  "Reads a bencoded .torrent file into a string."
  ;; Use ISO-8859-1 because it maps each byte value to a character, so
  ;; the mix of ASCII and raw bytes in a .torrent file won't cause an error.
  (uiop:read-file-string path :external-format :iso-8859-1))

(defun compute-info-hash (metainfo)
  "Extracts the 'info' field from a metainfo dict and computes its SHA1
hash, which is returned as a string."
  (compute-sha1 (bencode:bencode (bencode:dict-get metainfo "info"))))

(defun compute-sha1 (seq &key (output-type :bytes))
  "Returns SHA1 hash. If OUTPUT-TYPE is :BYTES, as a bytestring; if :HEX, as
a string of (lower case) hex digits."
  (case output-type
    (:bytes (coerce (mapcar #'code-char (sha1:sha1-digest seq)) 'string))
    (:hex (string-downcase (sha1:sha1-hex seq)))
    (t (error "Unknown encoding type."))))
