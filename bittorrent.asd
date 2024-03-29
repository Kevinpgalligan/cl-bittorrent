(defpackage :bittorrent-asd
  (:use :cl :asdf))

(in-package :bittorrent-asd)

(defsystem bittorrent
  :license "MIT"
  :author "Kevin Galligan"
  :description "A BitTorrent client."
  :depends-on (:esrap :trees :usocket :dexador
               :quri :sha1 :str :flexi-streams
               :bordeaux-threads :lparallel :log4cl)
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "bencode")
               (:file "queue")
               (:file "torrent")
               (:file "message")
               (:file "peer")
               (:file "tracker")
               (:file "piece")
               (:file "client")
               ))
