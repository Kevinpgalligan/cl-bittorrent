(in-package bittorrent-test)

(def-suite bencoding
  :in bittorrent)

(in-suite bencoding)

(defun is-reencodable (s)
  (is (string= s (bencode:bencode
                  (bencode:bdecode s)))))

(test single-digit-benteger (is-reencodable "i1e"))
(test multi-digit-benteger (is-reencodable "i12e"))
(test zero-benteger (is-reencodable "i0e"))
(test negative-benteger (is-reencodable "i-1e"))
(test minus-zero (signals error (bencode:bdecode "i-0e")))
(test leading-zero (signals error (bencode:bdecode "i01e")))
(test benteger-missing-prefix (signals error (bencode:bdecode "1e")))
(test benteger-missing-suffix (signals error (bencode:bdecode "i1")))

(test valid-string (is-reencodable "2:hi"))
(test empty-string (is-reencodable "0:"))
(test string-missing-colon (signals error (bencode:bdecode "2hi")))
(test string-wrong-length (signals error (bencode:bdecode "4:hi")))
(test string-missing-length (signals error (bencode:bdecode ":hi")))

(test list (is-reencodable "l2:hii123e4:spame"))
;; Maybe not testing thoroughly enough that alphabetic order is retained.
(test dict (is-reencodable "d2:ab4:spame"))
(test dict-missing-value (signals error (bencode:bdecode "d2:abe")))
(test nested-structures (is-reencodable "ld2:hi3:byeei123ee"))

;; Would be nice to test for weird unicode characters.
