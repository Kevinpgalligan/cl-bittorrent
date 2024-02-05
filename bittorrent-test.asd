(defpackage :bittorrent-test-asd
  (:use :cl :asdf))

(in-package :bittorrent-test-asd)

(defsystem bittorrent-test
  :license "MIT"
  :author "Kevin Galligan"
  :depends-on (:bittorrent :fiveam)
  :pathname "t"
  :serial t
  :components ((:file "package")
               (:file "bencode")
               (:file "message")
               (:file "piece")
               ))
