(defpackage :bencode
  (:use :cl :esrap))

(in-package bencode)

;; Test cases.
;;   1. i-0e is invalid.
;;   2. i03e is invalid.
;;   3. i0e is valid.
;;   4. i12e
;;   5. i-12e

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
    (parse-integer (format nil "~{~a~}" list))))o

;; Valid string encodings:
;;   2:hi
;;   0:
;; Invalid string encodings:
;;   2hi
;;   4:hi
;;   :hi

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
(defrule list (and "l" (* ben) "e") (:function second))
(defrule dict (and "d" (* (and str ben)) "e") (:function second))
