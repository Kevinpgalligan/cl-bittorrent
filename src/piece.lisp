;;;; Stitching together, writing, and validating pieces.

(in-package bittorrent)

(defclass piece ()
  ((index :initarg :index :reader index)
   (start :initarg :start :reader start)
   (end :initarg :end :reader end)
   (bytes :initarg :bytes :reader bytes)))

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
    ;; Annoying. I'm sure there's a way to do this.
    (cond
      ((null blocks) (setf blocks (list blok)))
      ((< (start blok) (start (first blocks)))
       (setf blocks (cons blok blocks)))
      (t (labels ((ins (current)
                    (if (or (null (cdr current))
                            (< (start blok) (start (cadr current))))
                        (setf (cdr current) (cons blok (cdr current)))
                        (ins (cdr current)))))
           (ins blocks))))))

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

(defun write-piece (piece files)
  "Writes a piece to the filesystem in the appropriate file(s)."
  (loop with rel-piece-pos = 0 ; pointer within piece
        ;; pointers across all torrent bytes
        with piece-pos = (start piece)
        with pos = 0
        while (< piece-pos (end piece))
        for file in files
        when (< piece-pos (+ pos (len file)))
          do (progn
               (incf rel-piece-pos (write-to-file piece
                                                  file
                                                  rel-piece-pos
                                                  (- piece-pos pos)))
               (setf piece-pos (+ rel-piece-pos (start piece))))
        do (incf pos (len file))))

(defun write-to-file (piece file rel-piece-pos file-pos)
  "Returns how many bytes were written.
REL-PIECE-POS is the relative position within the bytes of the piece, from
0 to NumBytesInPiece-1. FILE-POS is the position to write within the file, from
0 to NumBytesInFile-1."
  (with-open-file (fstream
                   (path file)
                   :direction :output
                   :element-type '(unsigned-byte 8))
    (file-position fstream file-pos)
    (let ((num-bytes (min (- (len file) file-pos)
                          (- (end piece) rel-piece-pos))))
      (write-sequence (bytes piece)
                      fstream
                      :start rel-piece-pos
                      :end (+ rel-piece-pos num-bytes))
      num-bytes)))
