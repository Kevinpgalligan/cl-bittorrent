;; A dummy tracker, used for integration testing. Ignores HTTP
;; parameters and just returns a dummy result (no peers).
;; To run, call slime-eval-buffer from within Emacs.
;; Note that data/for-tests.torrent directs to 127.0.0.1:4242/announce,
;; which is the address that this "tracker" listens on.

(ql:quickload 'hunchentoot)

(hunchentoot:define-easy-handler (handle-announce :uri "/announce"
                                                  :request-type :get) ()
  (concatenate
   'string
   "d"
   "8:intervali900e"
   "10:tracker id5:hello"
   "8:completei0e"
   "10:incompletei0e"
   "5:peersle"
   "e"))

(defparameter *acceptor*
  (make-instance 'hunchentoot:easy-acceptor
                 :name 'http
                 :port 4242
                 :access-log-destination nil
                 :message-log-destination nil))

(hunchentoot:start *acceptor*)

;; Call this to stop the tracker server:
;;   (hunchentoot:stop *acceptor*)
