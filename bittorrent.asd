(defpackage :bittorrent-asd
  (:use :cl :asdf))

(in-package :bittorrent-asd)

(defsystem bittorrent
  :license "MIT"
  :author "Kevin Galligan"
  :description "Implementation of the BitTorrent protocol."
  :depends-on (:esrap :trees :usocket :dexador :quri :sha1)
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "bencode")
               (:file "tracker")
               ))
