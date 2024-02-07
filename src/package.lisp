(defpackage :bittorrent
  (:use :cl)
  (:export
   :download-torrent

   ;; These are only exported for testing. I'm sure
   ;; there's a better way to do that, though.
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
