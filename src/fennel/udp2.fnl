(local json (require :dkjson))
(local sok (require :socket))

(fn listen [socket ops]
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
                (let [f (?. ops op)
                      ret (f (if (= input.id id) input.data data))]
                  (reset-input)
                  ret))))))))

(comment
 (do
   (local sok1 (assert (sok.udp)))
   (local sok2 (assert (sok.udp)))
   (local sok3 (assert (sok.udp)))
   (sok1:setoption "reuseaddr" true)
   (sok2:setoption "reuseaddr" true)
   (assert (sok1:setsockname "127.0.0.1" "55551"))
   (assert (sok2:setpeername "127.0.0.1" "55551"))
   (assert (sok3:setpeername "127.0.0.1" "8084"))
   (local view (require :fennel.view))
   (local go (listen sok1
                     {:print (fn [x] [x x])}))
   (go)
   (sok2:send (json.encode {:id 1 :data "hello "}))
   (sok2:send (json.encode {:id 1 :op :print}))
   (sok2:send (json.encode {:id 2 :op :print :data "pouet"}))
   ()))
