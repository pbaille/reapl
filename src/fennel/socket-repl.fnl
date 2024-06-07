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

(fn wrap-repl [options]
  (var repl-complete nil)
  (var ret nil)
  (fn send []
    (let [opts (collect [k x (pairs (or options {})) :into {}]
                 (values k x))]
      (fn opts.readChunk []
        (coroutine.yield ret))
      (fn opts.onValues [x]
        (set ret {:values x}))
      (fn opts.onError [e-type e lua-src]
        (set ret {:error {:type e-type :message e :src lua-src}}))
      (fn opts.registerCompleter [x]
        (set repl-complete x))
      (fn opts.pp [x] x)
      (set opts.error-pinpoint ["«" "»"])
      (fnl.repl opts)))
  (let [co (coroutine.create send)
        repl-send (fn [x] (coroutine.resume co x))
        repl-close (fn [] (coroutine.close co))]
    (repl-send)
    (values repl-send repl-complete repl-close)))

(fn error-handler [sok error-type]
  (fn [e]
    (sok:send
     (json.encode {:error {:type error-type :message e}} {}))))

(local repl-ops
       [:eval :complete :doc :reload :find :compile :apropos :apropos-doc :apropos-show-docs])

(fn repl-fn [{: debug}]
  (let [(send comp close) (wrap-repl)]
    (fn repl []
      (udp:settimeout 0.0001)
      (local m (udp:receive))
      (if m
          (do (if debug (log m))
              (let [{&as opts : op : arg} (json.decode m)]

                (case op

                  :eval
                  (xpcall (fn [] (let [(ok? ret) (send arg)]
                                   (xpcall (fn []
                                             (udp-out:send (json.encode {: op :expression arg :output ret} {})))
                                           (error-handler udp-out "encode"))))
                          (error-handler udp-out "eval"))

                  :complete
                  (udp-out:send (let [completions (comp arg)
                                      types (collect [_ v (ipairs completions)]
                                              (let [(ok? ret) (send (.. "(type " v ")"))]
                                                (values v (if (and ok? ret)
                                                              (. ret.values 1)
                                                              "unknown"))))]
                                  (json.encode {: op
                                                :symbol arg
                                                : completions
                                                : types} {})))
                  _
                  (if (u.seq.find repl-ops #(= op $))
                      (let [_ (log (.. "," op " " arg))
                            (ok? ret) (send (.. "," op " " arg))]
                        (udp-out:send
                         (json.encode
                          (u.tbl.merge opts
                                       {:output (if (and ok? ret)
                                                    ret
                                                    {:error {:type "repl:op" :message (or ret "op fail...")}})}))))
                      (udp-out:send
                       (json.encode
                        {:error {:type :unknow-op
                                 :message (.. "Reapl: '" op "' not supported.")}})))))))
      (reaper.defer repl))))

(fn start-repl [{&as options : ports}]
  (setup-udp options)
  ((repl-fn options)))
