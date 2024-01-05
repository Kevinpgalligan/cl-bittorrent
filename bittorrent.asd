(defpackage :bittorrent-asd
  (:use :cl :asdf))

(in-package :bittorrent-asd)

(defsystem sketches
  :license "MIT"
  :author "Kevin Galligan"
  :description "Implementation of the BitTorrent protocol."
  :depends-on (:esrap)
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "bencode")
               ))
