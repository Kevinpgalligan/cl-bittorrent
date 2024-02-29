(in-package bittorrent-test)

(def-suite piece
  :in bittorrent)

(in-suite piece)

(test stitching-blocks
  (let ((pp (make-partial-piece 0 0 10))
        (blocks (list (make-block 0 3 #(3 4 5 6 7))
                      (make-block 0 0 #(0 1 2))
                      (make-block 0 8 #(8 9)))))
    (loop for bl in blocks
          do (is (not (piece-ready-p pp)))
          do (block-insert pp bl))
    (is (piece-ready-p pp))
    (let ((piece (stitch-together-piece pp)))
      (is (= 0 (index piece)))
      (is (= 0 (start piece)))
      (is (= 10 (end piece)))
      (is (equalp #(0 1 2 3 4 5 6 7 8 9) (bytes piece)))
      (is
       (valid-piece-p
        ;; I happen to know that this is the SHA-1 hash...
        (coerce
         (mapcar #'code-char
                 '(73 65 121 113 74 108 214 39 35 157 254 222 223 45 233 239 153 76 175 3))
         'string)
        piece)))))

(test all-blocks
  (let ((torrent
          (make-instance 'torrent
                         :metainfo nil
                         :tracker-list nil
                         :info-hash nil
                         :dirname nil
                         :files nil
                         :total-length 987
                         :piece-length 100
                         :piece-hashes nil
                         :num-pieces 10
                         :max-block-size 30)))
    (is (equalp '((:piece-index 0
                   :begin 0
                   :length 30)
                  (:piece-index 0
                   :begin 30
                   :length 30)
                  (:piece-index 0
                   :begin 60
                   :length 30)
                  (:piece-index 0
                   :begin 90
                   :length 10))
                (bito::all-blocks torrent 0)))
    ;; Last piece gets cut short, has 87 bytes.
    (is (equalp '((:piece-index 9
                   :begin 0
                   :length 30)
                  (:piece-index 9
                   :begin 30
                   :length 30)
                  (:piece-index 9
                   :begin 60
                   :length 27))
                (bito::all-blocks torrent 9)))))
