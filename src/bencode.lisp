(defpackage :bencode
  (:use :cl :esrap)
  (:export
   :bencode
   :bdecode
   :dict-get))

(in-package bencode)

(defrule nonzero (or "1" "2" "3" "4" "5" "6" "7" "8" "9"))
(defrule digit (or "0" nonzero))

(defrule benteger
    (and #\i
         (or "0"
             (and (? #\-)
                  nonzero
                  (* digit)))
         #\e)
  (:lambda (list)
    (let ((inner-part (second list)))
      (if (stringp inner-part)
          0
          (funcall (if (first inner-part) #'- #'+)
                   (parse-integer (format nil "~{~a~}"
                                          (alexandria:flatten (rest inner-part)))))))))

(defrule integer
    (or "0" (and nonzero (* digit)))
  (:function alexandria:flatten)
  (:lambda (list)
    (parse-integer (format nil "~{~a~}" list))))

(defun parse-bencoded-string (text position end)
  (multiple-value-bind (size new-position)
      (parse 'integer text :start position :end end :junk-allowed t)
    (when (not size)
      (return-from parse-bencoded-string
        (values nil position "Failed to parse integer length prefix.")))
    ;; Make sure the next character is a colon.
    (when (not (parse #\: text :start new-position :end end :junk-allowed t))
      (return-from parse-bencoded-string
        (values nil new-position "Missing colon separator.")))
    (let* ((start-index (1+ new-position))
           (end-index (+ start-index size)))
      (when (> end-index end)
        (error "String length goes beyond parsing region."))
      (values (subseq text start-index end-index)
              end-index
              t))))

(defrule str (function parse-bencoded-string))
(defrule ben (or str benteger list dict))
(defrule list (and "l" (* ben) "e")
  (:function second))
(defrule dict (and "d" (* (and str ben)) "e")
  (:lambda (list)
    ;; Nodes in the tree are pairs of key and value.
    ;; Keys must be strings. Using a tree ensures that
    ;; they are stored in sorted order, as required by
    ;; the BitTorrent spec.
    (let ((d (trees:make-binary-tree :normal
                                     'string<
                                     :key 'first
                                     :test 'string=)))
      (loop for pair in (second list)
            do (trees:insert pair d))
      d)))

(defun bdecode (string)
  (parse 'ben string))

(defun bencode (data)
  (labels
      ((encode (data outstream)
         (cond
           ((integerp data)
            (format outstream "i~ae" data))
           ((stringp data)
            (format outstream "~a:~a" (length data) data))
           ((listp data)
            (write-string "l" outstream)
            ;; Not ideal to use recursion, could blow the stack.
            ;; But it'll do for now.
            (loop for item in data
                  do (encode item outstream))
            (write-string "e" outstream))
           ((eq (type-of data) 'binary-trees:binary-tree)
            (write-string "d" outstream)
            (trees:dotree (node data)
              (encode (first node) outstream)
              (encode (second node) outstream))
            (write-string "e" outstream))
           (t (error "Unknown data type.")))))
    (with-output-to-string (s)
      (encode data s))))

(defun dict-get (dict key)
  (second (trees:find key dict)))
