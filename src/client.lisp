(in-package bittorrent)

(defun run-client (torrent-path download-path)
  (let ((torrent (load-torrent-file torrent-path)))
    (multiple-value-bind (sock port) (attempt-open-bittorrent-socket)
      (multiple-value-bind (tracker-response peer-id)
          (query-tracker torrent :port port :event :started)
        (let* ((peer
                 (alexandria:random-elt (peers tracker-response)))
               (peer-sock (connect-to-peer peer)))
          (send-handshake torrent peer-id peer-sock)
          (if (wait-handshake peer peer-sock torrent)
              (format t "Handshake successful!~%")
              (format t "Handshake FAILED :(~%")))))))

(defun attempt-open-bittorrent-socket ()
  (or
   (loop for port from 6881 below 6890
         for sock = (handler-case (usocket:socket-listen
                                   "127.0.0.1" port :element-type 'flexi-streams:octet)
                      (usocket:address-in-use-error ()
                        nil))
         when sock
           do (return (values sock port)))
   (error "Couldn't find a free port in the range 6881-6889.")))

