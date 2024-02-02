(ql:quickload 'bittorrent)

(in-package bittorrent)

(defparameter *torrent-path* "./data/sample.torrent")

(bt:make-thread
 (lambda ()
   (peer-loop (load-torrent-file *torrent-path*)
              ;; ID here
              ;; peer here
              )))
