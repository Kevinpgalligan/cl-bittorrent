(in-package bittorrent-test)

(def-suite message
    :in bittorrent)

(in-suite message)

(defun store-and-check (list-of-packets expected-messages
                        &optional (num-pieces 8))
  (let ((msg-buff (make-message-buffer num-pieces 100)))
    (loop for bytes in list-of-packets
          for emsgs in expected-messages
          do (let ((msgs (mb-store msg-buff bytes)))
               (is (= (length emsgs) (length msgs)))
               (loop for msg in msgs
                     for expected-msg in emsgs
                     do (is (message= msg expected-msg)))))))

(defun send-packets (list-of-packets &optional (num-pieces 8))
  (let ((msg-buff (make-message-buffer num-pieces 100)))
    (loop for bytes in list-of-packets
          do (mb-store msg-buff bytes))))

(test read-bytes
  (let ((msg-buff (make-message-buffer 5 100))
        (packet #(0 1 2 3 4 5 6)))
    (is (= 5 (read-in-bytes 5 msg-buff packet 0)))
    (is (equalp #(0 1 2 3 4) (bytes msg-buff)))
    (is (= 2 (read-in-bytes nil msg-buff packet 5)))
    (is (equalp #(0 1 2 3 4 5 6) (bytes msg-buff)))))

(test parse-message-len
  (let ((msg-buff (make-message-buffer 5 100))
        (packet #(0 0 1 1)))
    (read-in-bytes 4 msg-buff packet 0)
    (is (= 257 (parse-message-len msg-buff packet 4)))))

(test parse-message-len-across-buffer-and-packet
  (let ((msg-buff (make-message-buffer 5 100))
        (packet #(0 0 1 1)))
    (read-in-bytes 2 msg-buff packet 0)
    (is (= 257 (parse-message-len msg-buff packet 2)))))

(test keepalive
  (store-and-check '(#(0 0 0 0))
                   (list (list (make-message :id :keep-alive)))))

(test not-enough-bytes
  (store-and-check '(#(0 0 0))
                   (list (list))))

(test keepalive-over-two-packets
      (store-and-check '(#(0 0 0) #(0))
                       (list (list)
                             (list (make-message :id :keep-alive)))))

(test two-messages-in-a-packet
  (store-and-check '(#(0 0 0 0 0 0 0 0))
                   (list (list (make-message :id :keep-alive)
                               (make-message :id :keep-alive)))))

(test simple-state-messages-spread-over-packets
  (store-and-check '(#(0 0 0 1 0
                       0 0)
                     #(0 1 1
                       0 0 0 1 2
                       0 0 0)
                     #(1 3))
                   (list (list (make-message :id :choke))
                         (list (make-message :id :unchoke)
                               (make-message :id :interested))
                         (list (make-message :id :not-interested)))))

(test have-message
  (store-and-check '(#(0 0 0 5 4 2 0 0 1))
                   (list (list (make-message :id :have
                                             :data 33554433)))))

(test request-message
  (store-and-check '(#(0 0 0 13 ; length
                       6 ; id
                       0 0 1 1 ; index
                       0 0 1 13 ; begin
                       0 2 0 0)) ; length
                   (list (list
                          (make-message :id :request
                                        :data '(:index 257
                                                :begin 269
                                                :length 131072))))))

(test cancel-message
  (store-and-check '(#(0 0 0 13
                       8
                       0 0 1 1
                       0 0 1 13
                       0 2 0 0))
                   (list (list
                          (make-message :id :cancel
                                        :data '(:index 257
                                                :begin 269
                                                :length 131072))))))

(test bitfield-message
  (store-and-check '(#(0 0 0 2 5 174))
                   (list (list (make-message :id :bitfield
                                             :data #*10101110)))))

(test bitfield-message-with-padding
  (store-and-check '(#(0 0 0 2 5 174))
                   (list (list (make-message :id :bitfield
                                             :data #*1010111)))
                   7))

(test bitfield-message-multiple-bytes
  (store-and-check '(#(0 0 0 3 5 174 240))
                   (list (list (make-message :id :bitfield
                                             :data #*101011101111000)))
                   15))

(test piece-message
  (store-and-check '(#(0 0 0 19 7
                       0 0 1 2
                       0 1 0 0
                       1 2 3 4 5 6 7 8 9 10))
                   (list (list (make-message :id :piece
                                             :data '(:index 258
                                                     :begin 65536
                                                     :block #(1 2 3 4 5 6 7 8 9 10)))))))

(test len-too-long (signals error (send-packets '(#(255 100 10 200)))))
(test unknown-id (signals error (send-packets '(#(0 0 0 1 9)))))
(test bitfield-too-short (signals error
                           (send-packets '(#(0 0 0 2 5 1))
                                         20)))
(test bitfield-too-long (signals error
                          (send-packets '(#(0 0 0 3 5 1 2)))))


(defun survives-roundtrip (msg)
  (let ((msg-buff (make-message-buffer 10 200))
        (bytes (make-array 100 :fill-pointer 0 :adjustable t)))
    (serialise-message msg bytes)
    (message= msg (first (mb-store msg-buff bytes)))))

(test roundtrip-keepalive (survives-roundtrip (make-message :id :keep-alive)))
(test roundtrip-choke (survives-roundtrip (make-message :id :choke)))
(test roundtrip-unchoke (survives-roundtrip (make-message :id :unchoke)))
(test roundtrip-interested (survives-roundtrip (make-message :id :interested)))
(test roundtrip-not-interested (survives-roundtrip (make-message :id :not-interested)))
(test roundtrip-have
      (survives-roundtrip
       (make-message :id :have :data 1231344)))
(test roundtrip-request
      (survives-roundtrip
       (make-message :id :request
                     :data '(:index 0
                             :begin 424242
                             :length 12345))))
(test roundtrip-cancel
      (survives-roundtrip
       (make-message :id :cancel
                     :data '(:index 0
                             :begin 123144
                             :length 1))))
(test roundtrip-bitfield
      (survives-roundtrip
       (make-message :id :bitfield :data #*0100101001)))
(test roundtrip-piece
      (survives-roundtrip
       (make-message :id :piece
                     :data '(:index 12
                             :begin 12345
                             :block #(1 2 3 4 5 6 7 8 9 10)))))
