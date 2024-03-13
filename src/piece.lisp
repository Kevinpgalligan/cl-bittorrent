;;;; Stitching together, writing, and validating pieces.

(in-package bittorrent)

(defclass piece ()
  ((index :initarg :index :reader index)
   (start :initarg :start :reader start)
   (end :initarg :end :reader end)
   (bytes :initarg :bytes :reader bytes)))

(defmethod size ((piece piece))
  (- (end piece) (start piece)))

(defun make-piece (index start end bytes)
  (make-instance 'piece :index index :start start :end end :bytes bytes))

(defclass blok ()
  ((piece-index :initarg :piece-index :reader piece-index)
   (start :initarg :start :reader start)
   (bytes :initarg :bytes :reader bytes)))

(defun make-block (piece-index start bytes)
  (make-instance 'blok :piece-index piece-index :start start :bytes bytes))

(defun block-end (blok)
  (+ (start blok) (block-size blok)))

(defun block-size (blok)
  (length (bytes blok)))

(defclass partial-piece ()
  ((piece-index :initarg :piece-index :reader piece-index)
   (start :initarg :start :reader start)
   (end :initarg :end :reader end)
   (size :initarg :size :reader size)
   (bytecount :initform 0 :accessor bytecount)
   (blocks :initform nil :accessor blocks)))

(defun make-partial-piece (piece-index start end)
  (make-instance 'partial-piece
                 :piece-index piece-index
                 :start start
                 :end end
                 :size (- end start)))

(defun block-insert (partial-piece blok)
  "Inserts a block into the correct position in the list of blocks
accumulated for this piece. Assumes that the block has been validated, i.e.
that its start & end are in the appropriate range, that it doesn't overlap
with other blocks, etc."
  (incf (bytecount partial-piece) (block-size blok))
  (with-slots (blocks) partial-piece
    ;; Annoying. I'm sure this is implemented somewhere already.
    (if (or (null blocks)
            (< (start blok) (start (first blocks))))
        (setf blocks (cons blok blocks))
        (labels ((ins (current)
                    (if (or (null (cdr current))
                            (< (start blok) (start (cadr current))))
                        (setf (cdr current) (cons blok (cdr current)))
                        (ins (cdr current)))))
           (ins blocks)))))

(defun piece-ready-p (partial-piece)
  (= (bytecount partial-piece) (size partial-piece)))

(defun stitch-together-piece (partial-piece)
  (let ((stream (flexi-streams:make-in-memory-output-stream)))
    (map nil
         (lambda (blok) (write-sequence (bytes blok) stream))
         (blocks partial-piece))
    (make-piece
     (piece-index partial-piece)
     (start partial-piece)
     (end partial-piece)
     (flexi-streams:get-output-stream-sequence stream))))

(defun valid-piece-p (expected-hash piece)
  (string= expected-hash (compute-sha1 (bytes piece))))

(defun write-piece (piece filespecs)
  "Writes a piece to the filesystem in the appropriate file(s)."
  (loop for fr in (get-file-ranges filespecs (start piece) (end piece))
        do (write-to-file-range fr piece)))

(defun write-to-file-range (fr piece)
  (with-open-file (stream
                   (path fr)
                   :direction :output
                   :if-exists :overwrite
                   :if-does-not-exist :create
                   :element-type '(unsigned-byte 8))
    (file-position stream (relative-start fr))
    (write-sequence (bytes piece)
                    stream
                    :start (- (max (absolute-start fr) (start piece))
                              (start piece))
                    :end (- (min (absolute-end fr) (end piece))
                            (start piece)))))

(defclass file-range ()
  ((path :initarg :path :reader path)
   (relative-start :initarg :relative-start :reader relative-start)
   (relative-end :initarg :relative-end :reader relative-end)
   (absolute-start :initarg :absolute-start :reader absolute-start)
   (absolute-end :initarg :absolute-end :reader absolute-end)
   (len :initarg :len :reader len)))

(defmethod print-object ((obj file-range) stream)
  (format stream
          "#<FILE-RANGE path=~a relstart=~a relend=~a absstart=~a absend=~a len=~a"
          (path obj)
          (relative-start obj)
          (relative-end obj)
          (absolute-start obj)
          (absolute-end obj)
          (len obj)))

(defun load-bytes-from-files (torrent start end)
  "Reads bytes from torrented files between START and END, which are indexes
into the torrent data when laid out contiguously on the file system. This
range might include multiple files. Assumes that the files exist, which they
should if the piece has been written there already."
  (let* ((frs (get-file-ranges (files torrent) start end))
         (buffer (make-array (reduce #'+ frs :key #'len :initial-value 0)
                             :element-type '(unsigned-byte 8))))
    (log:debug "Loading bytes: ~a" frs)
    (loop with i = 0
          for fr in frs
          do (read-file-range-into-buffer fr buffer i)
          do (incf i (len fr)))
    buffer))

(defun read-file-range-into-buffer (fr buffer buffer-ptr)
  (with-open-file (stream (path fr) :element-type '(unsigned-byte 8))
    (file-position stream (relative-start fr))
    (read-sequence buffer
                   stream
                   :start buffer-ptr
                   :end (+ buffer-ptr (len fr)))))

(defun get-file-ranges (filespecs start end)
  (loop with file-start = 0
        for filespec in filespecs
        while (< file-start end)
        for file-end = (+ file-start (len filespec))
        for overlap = (ranges-overlap file-start file-end start end)
        when overlap
          collect (let ((s (first overlap))
                        (e (second overlap)))
                    (make-instance 'file-range
                                   :path (path filespec)
                                   :absolute-start s
                                   :absolute-end e
                                   :relative-start (- s file-start)
                                   :relative-end (- e file-start)
                                   :len (- e s)))
        do (incf file-start (len filespec))))

(defun ranges-overlap (s1 e1 s2 e2)
  "Returns '(start end) or nil. Starts are inclusive, ends exclusive."
  (let ((overlap
          (cond
            ((and (<= s1 s2) (<= e2 e1))
             (list s2 e2))
            ((and (<= s2 s1) (<= e1 e2))
             (list s1 e1))
            ((and (<= s1 s2) (<= s2 e1))
             (list s2 e1))
            ((and (<= s1 e2) (<= e2 e1))
             (list s1 e2)))))
    (if (and overlap (= (first overlap) (second overlap)))
        nil
        overlap)))
