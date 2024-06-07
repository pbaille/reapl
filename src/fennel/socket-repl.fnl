(local sok (require :reaper-socket))
(local json (require :dkjson))
(local fnl (require :fennel))

(global u (require :pb-utils))
(global ru (require :reaper-utils))

(local r reaper)
(local log ru.misc.log)

(local udp (assert (sok.udp)))
(local udp-out (assert (sok.udp)))

(fn setup-udp [{:ports {: input : output}}]
  (udp:setoption "reuseaddr" true)
  (assert (udp:setsockname "127.0.0.1" input))
  (assert (udp-out:setpeername "127.0.0.1" output))
  (ru.misc.log (.. "setting udp sockets:\n"
                   "in: localhost:" input
                   "\nout: localhost:" output
                   "\n\nReapl server running !\n")))

(var result nil)
(var debug nil)

(fn dbg [x]
    (if debug
        (log (fnl.view x))))

(fn wrap-repl [options]
  (var repl-complete nil)
  (fn send []
    (let [opts (collect [k x (pairs (or options {})) :into {}]
                 (values k x))]
      (fn opts.readChunk [x]
        (dbg [:readChunk x])
        (coroutine.yield x))
      (fn opts.onValues [x]
        (dbg [:onValues x])
        (set result {:values x}))
      (fn opts.onError [e-type e lua-src]
        (dbg [:onError {: e-type : e : lua-src}])
        (set result {:error {:type e-type :message e :src lua-src}}))
      (fn opts.registerCompleter [x]
        (set repl-complete x))
      (fn opts.pp [x] x)
      (set opts.error-pinpoint ["«" "»"])
      (fnl.repl opts)))
  (let [repl-send (coroutine.wrap send)]
    (repl-send)
    (values repl-send repl-complete ret)))

(fn error-handler [command error-type]
  (fn [e]
    (udp-out:send
     (json.encode (u.tbl.merge command {:error {:type error-type :message e}}) {}))))

(local repl-ops
       [:eval :complete :doc :reload :find :compile :apropos :apropos-doc :apropos-show-docs])

(fn repl-fn []
  (let [(send comp) (wrap-repl)]
    (fn repl []
      (udp:settimeout 0.0001)
      (local m (udp:receive))
      (if m
          (do (set result nil)
              (dbg [:input m])
              (let [{&as opts : op : arg} (json.decode m)]

                (case op

                  :eval
                  (xpcall (fn [] (let [_ (send arg)]
                                   (dbg [:eval result])
                                   (xpcall (fn []
                                             (udp-out:send (json.encode {: op :expression arg :output result} {})))
                                           (error-handler opts "encode"))))
                          (error-handler opts "eval"))

                  :complete
                  (udp-out:send (let [completions (comp arg)
                                      types (collect [_ v (ipairs completions)]
                                              (let [_ (send (.. "(type " v ")"))]
                                                (values v (if result
                                                              (. result.values 1)
                                                              "unknown"))))]
                                  (json.encode {: op
                                                :symbol arg
                                                : completions
                                                : types} {})))
                  _
                  (if (u.seq.find repl-ops #(= op $))
                      (let [_ (send (.. "," op " " arg "\n"))]
                        (udp-out:send
                         (json.encode
                          (u.tbl.merge opts
                                       {:output (or result
                                                    {:error {:type "repl:op" :message "op fail..."}})}))))
                      (udp-out:send
                       (json.encode
                        {:error {:type :unknow-op
                                 :message (.. "Reapl: '" op "' not supported.")}})))))))
      (reaper.defer repl))))

(fn start-repl [{&as options : ports}]
  (setup-udp)
  ((repl-fn options)))
