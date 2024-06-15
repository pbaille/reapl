(local json (require :dkjson))

(fn listen [socket handler]
  (var input {:data ""})
  (fn reset-input []
    (set input {:data ""}))
  (socket:settimeout 0.0001)
  (fn loop []
    (let [m (socket:receive)]
      (when m
        (let [{&as json-message : id : op : data} (json.decode m)]
          (when id
            (if (not (= input.id id))
                (reset-input))
            (if (and data (not op))
                (do (set input {: id :data (.. input.data data)})
                    (loop))
                (do (when (= input.id id)
                      (tset json-message :data input.data))
                    (let [ret (handler json-message)]
                      (reset-input)
                      ret)))))))))

(comment

 (local sok (require :socket))
 (local sok1 (assert (sok.udp)))
 (local sok2 (assert (sok.udp)))
 (assert (sok1:setsockname "127.0.0.1" "55551"))
 (assert (sok2:setpeername "127.0.0.1" "55551"))
 (local go (listen sok1
                   (fn [{: op : data}]
                     (case op
                       :print (do (print data)
                                  [data data])
                       _ :pouetpouet))))
 (go)
 (sok2:send (json.encode {:id 1 :data "hello "}))
 (sok2:send (json.encode {:id 1 :op :print}))
 (sok2:send (json.encode {:id 2 :op :print :data "pouet"}))

 (local sok3 (assert (sok.udp)))
 (assert (sok3:setpeername "127.0.0.1" "8088"))
 (sok3:send (json.encode {:id 1 :data "hello "}))
 (sok3:send (json.encode {:id 1 :op :print})))

{: listen}
