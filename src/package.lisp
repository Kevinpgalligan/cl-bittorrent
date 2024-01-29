(defpackage :bittorrent
  (:use :cl)
  (:export
   :make-message-buffer
   :mb-store
   :make-message
   :message=
   :bytes
   :read-in-bytes
   :parse-message-len
   :serialise-message
   :id))
