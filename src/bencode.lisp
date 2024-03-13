;;;; Encoding and decoding of the bencode format.

(defpackage :bencode
  (:use :cl :esrap)
  (:export
   :bencode
   :bencode-to-stream
   :bdecode
   :dict-p
   :dict-get
   :dict-has))

(in-package bencode)

(defparameter *on-missing-dict-entry* :return-nil
  "Can be either :RETURN-NIL or :ERROR.")

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
  "Decodes bencoded data. Output will be an integer, string, list, or
whatever data structure is used to represent dicts."
  (parse 'ben string))

(defun bencode-to-stream (data outstream)
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
           do (bencode-to-stream item outstream))
     (write-string "e" outstream))
    ((eq (type-of data) 'binary-trees:binary-tree)
     (write-string "d" outstream)
     (trees:dotree (node data)
                   (bencode-to-stream (first node) outstream)
                   (bencode-to-stream (second node) outstream))
     (write-string "e" outstream))
    (t (error "Unknown data type."))))

(defun bencode (data)
  "Encodes DATA -- which can be an integer, string, list, whatever
data structure is used to represent a dict, or any nested combination of
those -- in the bencoding format. Output is a string."
  (with-output-to-string (s)
    (bencode-to-stream data s)))

(defun dict-p (x)
  (eq 'trees:binary-tree (class-name (class-of x))))

(defun dict-get (dict &rest keys)
  "Get data from nested dicts. KEYS should be strings. Depending on the value
of *on-missing-dict-entry*, returns NIL if any of the keys are missing, or
signals an error."
  (loop for key in keys
        while dict
        do (let ((node (trees:find key dict)))
             (if (and (null node)
                      (eq :error *on-missing-dict-entry*))
                 (error (format nil "Missing key '~a'." key))
                 (setf dict (second node)))))
  dict)

(defun dict-has (dict &rest keys)
  (let ((*on-missing-dict-entry* :return-nil))
    (apply #'dict-get dict keys)))
