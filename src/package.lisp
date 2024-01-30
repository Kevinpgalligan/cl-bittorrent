(defpackage :bittorrent
  (:use :cl)
  (:export
   ;; This is for testing, the client does not 
   :make-message-buffer
   :mb-store
   :make-message
   :message=
   :bytes
   :read-in-bytes
   :parse-message-len
   :serialise-message
   :id
   :start
   :end
   :bytes
   :index
   :stitch-together-piece
   :piece-ready-p
   :block-insert
   :make-block
   :make-partial-piece
   :valid-piece-p))
