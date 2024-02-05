(defpackage :bittorrent
  (:use :cl)
  (:export
   :make-message-buffer
   :mb-store
   :make-message
   :message=
   :bytes
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
