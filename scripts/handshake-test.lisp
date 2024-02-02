(in-package bittorrent)

(defparameter *torrent-path*
  "/home/kg/proyectos/cl-bittorrent/data/sample.torrent")

(defparameter *id1* (random-peer-id))
(defparameter *id2* (random-peer-id))
(defparameter *q1* (make-queue))
(defparameter *q2* (make-queue))
(defparameter *control-queue* (make-queue))

(defparameter *listen-socket*
  (usocket:socket-listen "127.0.0.1"
                         6881
                         :element-type 'flexi-streams:octet))
(defparameter *sock1*
  (usocket:socket-connect "127.0.0.1"
                          6881
                          :element-type 'flexi-streams:octet))
(defparameter *sock2*
  (usocket:socket-accept *listen-socket*
                         :element-type 'flexi-streams:octet))

(bt:make-thread
 (lambda ()
   (peer-loop (load-torrent-file *torrent-path*)
              *id1*
              *q1*
              *control-queue*
              :sock *sock1*)))

(bt:make-thread
 (lambda ()
   (peer-loop (load-torrent-file *torrent-path*)
              *id2*
              *q2*
              *control-queue*
              :sock *sock2*)))

;; Wait to see if they explode due to handshake or something.
(format t "Waiting for messages.~%")
(sleep 3)
(format t "Checking queue for messages, shouldn't be any...~%")
(loop for msg = (qpop *control-queue*)
      while msg
      do (format t "~a~%" msg))
(format t "Done!~%")

;; Now send a message from one to the other.
(qpush *q1*
       (queue-message :tag :peer-message
                      :contents (make-message :id :unchoke)))
(sleep 1)
(format t "Checking queue again, expecting a choke!~%")
(loop for msg = (qpop *control-queue*)
      while msg
      do (format t "~a~%" msg))
(format t "Done.~%")

(format t "Shutting down threads.~%")
(qpush *q1* (queue-message :tag :shutdown))
(qpush *q2* (queue-message :tag :shutdown))
(sleep 1)
(format t "Checking queue again, expecting shutdowns!~%")
(loop for msg = (qpop *control-queue*)
      while msg
      do (format t "~a~%" msg))
(format t "Done.~%")
(format t "Threads: ~a~%" (bt:all-threads))
(format t "Closing sockets.~%")
(usocket:socket-close *listen-socket*)
(usocket:socket-close *sock1*)
(usocket:socket-close *sock2*)
