;;;; Communication with peers.

(in-package bittorrent)

(defclass peer ()
  ((ip :initarg :ip :accessor ip)
   (port :initarg :port :accessor port)
   (id :initarg :id :accessor id)))

(defun connect-to-peer (peer)
  (usocket:socket-connect (ip peer) (port peer)))

(defparameter *handshake-header*
  (concatenate 'vector
               #(19)
               (flexi-streams:string-to-octets "BitTorrent protocol") 
               #(0 0 0 0 0 0 0 0)))

(defun send-handshake (torrent peer-id sock)
  (let ((stream (usocket:socket-stream sock)))
    (write-sequence *handshake-header* stream)
    (write-string (info-hash torrent) stream)
    (write-string peer-id stream)
    (force-output stream)))

(defun write-bytes-from-string (string stream)
  (write-sequence (flexi-streams:string-to-octets string) stream))

(defun wait-handshake (peer sock torrent)
  (multiple-value-bind (buffer length)
      (usocket:socket-receive sock nil nil)
    (if (bytes-match? buffer (list *handshake-header* (info-hash torrent) (id peer)))
        t
        (progn
          (usocket:socket-close sock)
          nil))))

(defun bytes-match? (buffer bytes-and-strings)
  (loop for i = 0
        for x in bytes-and-strings
        when (or (< (length buffer) (+ i (length x)))
                 (mismatch buffer
                           (if (stringp x) (flexi-streams:string-to-octets x) x)
                           :start1 i
                           :end1 (+ i (length x))))
          do (return nil)
        do (incf i (length x))
        finally (return (= i (length buffer)))))
