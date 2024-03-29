;;;; Loading and manipulating .torrent files.

(in-package bittorrent)

;; Chosen to be the same as the max request size.
(defparameter *max-block-size* 16384)

(defclass torrent ()
  ((metainfo :initarg :metainfo :accessor metainfo)
   (tracker-list :initarg :tracker-list :accessor tracker-list)
   (info-hash :initarg :info-hash :accessor info-hash)
   (dirname :initarg :dirname :reader dirname)
   (files :initarg :files :reader files)
   (total-length :initarg :total-length :accessor total-length)
   (piece-length :initarg :piece-length :reader piece-length)
   (piece-hashes :initarg :piece-hashes :reader piece-hashes)
   (num-pieces :initarg :num-pieces :reader num-pieces)
   (max-block-size :initarg :max-block-size
                   :reader max-block-size
                   :initform *max-block-size*)))

(defun all-blocks (torrent piece-index)
  "Divides a piece up into blocks, returns a list of plists where
each plist has :BEGIN and :LENGTH fields indicating the (relative) start
index of that block within the piece and the length of the block."
  (with-slots (piece-length total-length)
      torrent
    (let* ((start-index (* piece-length piece-index))
           (this-piece-length (min piece-length
                                   (- total-length start-index))))
      (loop for relative-i = 0 then (+ relative-i (max-block-size torrent))
            while (< relative-i this-piece-length)
            collect (list :piece-index piece-index
                          :begin relative-i
                          :length (min (max-block-size torrent)
                                       (- this-piece-length relative-i)))))))

(defclass filespec ()
  ((path :initarg :path :accessor path)
   (len :initarg :len :reader len)))

(defun make-filespec (path len)
  (make-instance 'filespec :path path :len len))

(defun add-base-path (base-path filespec)
  (setf (path filespec) (uiop:merge-pathnames* (path filespec) base-path)))

(defun extract-tracker-list (metainfo)
  (if (bencode:dict-has metainfo "announce-list")
      (first (bencode:dict-get metainfo "announce-list"))
      (list (bencode:dict-get metainfo "announce"))))

(defun load-torrent-file (path)
  "Loads and parses a bencoded .torrent file."
  (let* ((metainfo (bencode:bdecode (read-torrent-to-string path)))
         (info (bencode:dict-get metainfo "info"))
         (files (extract-files metainfo info))
         (hashes (extract-piece-hashes metainfo)))
    (make-instance 'torrent
                   :metainfo metainfo
                   :tracker-list (extract-tracker-list metainfo)
                   :info-hash (compute-info-hash metainfo)
                   :dirname (and (multifile-mode-p metainfo)
                                 (bencode:dict-get info "name"))
                   :files files
                   :total-length (reduce #'+ (mapcar #'len files) :initial-value 0)
                   :piece-length (bencode:dict-get info "piece length")
                   :piece-hashes hashes
                   :num-pieces (length hashes))))

(defun multifile-mode-p (metainfo)
  (not (bencode:dict-has metainfo "info" "length")))

(defun extract-files (metainfo info)
  (if (multifile-mode-p metainfo)
      (let ((dir (concatenate 'string
                              (bencode:dict-get info "name")
                              "/")))
        (mapcar (lambda (raw-filespec)
                  (make-filespec
                   (uiop:merge-pathnames*
                    (str:join "/" (bencode:dict-get raw-filespec "path"))
                    dir)
                   (bencode:dict-get raw-filespec "length")))
                (bencode:dict-get info "files")))
      (list (make-filespec (bencode:dict-get info "name")
                           (bencode:dict-get info "length")))))

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
