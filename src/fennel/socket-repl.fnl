(local sok (require :reaper-socket))
(local json (require :dkjson))
(local fnl (require :fennel))

(global u (require :pb-utils))
(global ru (require :reaper-utils))

(local r reaper)

(local udp (assert (sok.udp)))
(local udp-eval-out (assert (sok.udp)))
(local udp-complete-out (assert (sok.udp)))

(fn setup-udp [{: socketPort : evalPeerPort : completePeerPort : noConsole}]
  (udp:setoption "reuseaddr" true)
  (assert (udp:setsockname "127.0.0.1" socketPort))
  (assert (udp-eval-out:setpeername "127.0.0.1" evalPeerPort))
  (assert (udp-complete-out:setpeername "127.0.0.1" completePeerPort))
  (ru.misc.log (.. "setting udp sockets:\n"
                   "in: localhost:" socketPort
                   "\neval-out: localhost:" evalPeerPort
                   "\ncomplete-out: localhost:" completePeerPort
                   "\n\nReapl server running !\n")))

(fn wrap-repl [options]
  (var repl-complete nil)
  (var ret nil)
  (fn send []
    (let [log ru.misc.log
          opts (collect [k x (pairs (or options {})) :into {:useMetadata true}]
                 (values k x))]
      (fn opts.readChunk []
        (coroutine.yield ret))
      (fn opts.onValues [x]
        (set ret {:values x}))
      (fn opts.onError [e-type e lua-src]
        (set ret {:error {:type e-type :msg e :src lua-src}}))
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

(fn repl-fn [{: noConsole}]
  (let [(send comp close) (wrap-repl)]
    (fn repl []
      (udp:settimeout 0.0001)
      (local m (udp:receive))
      (if m
          (let [{: eval : complete} (json.decode m)]
            (if eval
                (xpcall (fn [] (let [(ok? ret) (send eval)]
                                 (xpcall (fn []
                                           (udp-eval-out:send (json.encode {:expression eval :output ret} {})))
                                         (error-handler udp-eval-out "encode"))))
                        (error-handler udp-eval-out "eval"))
                complete
                (xpcall (fn [] (udp-complete-out:send (let [completions (comp complete)
                                                            types (collect [_ v (ipairs completions)]
                                                                    (let [(ok? ret) (send (.. "(type " v ")"))]
                                                                      (values v (if ok? (. ret.values 1)
                                                                                    "unknown"))))]
                                                        (json.encode {:request complete
                                                                      : completions
                                                                      : types} {}))))
                        (error-handler udp-complete-out "encode")))))
      (reaper.defer repl))))

(fn start-repl [{&as options : socketPort : evalPeerPort : completePeerPort : noConsole}]
  (setup-udp options)
  ((repl-fn options)))
