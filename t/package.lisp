(defpackage bittorrent-test
  (:use :cl :fiveam :mockingbird :bittorrent)
  (:local-nicknames (#:bito #:bittorrent)))

(in-package bittorrent-test)

(def-suite bittorrent)
