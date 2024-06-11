(local sok (require :reaper-socket))
(local json (require :dkjson))

(global fennel (require :fennel))
(global u (require :pb-utils))
(global ru (require :reaper-utils))
(global reascript-doc (require :reascript-doc))
(global r reaper)

(fn dbg [...]
    (if debug
        (each [_ x (ipairs [...])]
          (ru.misc.log (if (= :string (type x))
                           x
                           (fennel.view x))))))

(var debug true)

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

(fn error-handler [command error-type]
  (fn [e]
    (udp-out:send
     (json.encode (u.tbl.merge command {:output {:error {:type error-type :message e}}}) {}))))

(local repl-ops
       [:eval :complete :doc :reload :find :compile :apropos :apropos-doc :apropos-show-docs])

(fn repl-fn []
  (let [{: complete : eval &as ops} (require :simple-repl)]
    (fn repl []
      (udp:settimeout 0.0001)
      (local m (udp:receive))
      (if m
          (do (dbg [:input m])
              (let [{&as opts : op : arg} (json.decode m)]

                (case op

                  :eval
                  (xpcall (fn [] (let [output (eval arg)]
                                   (dbg [:eval output])
                                   (xpcall (fn []
                                             (udp-out:send (json.encode {: op : arg : output} {})))
                                           (error-handler opts "encode"))))
                          (error-handler opts "eval"))

                  :complete
                  (udp-out:send (let [completions (complete arg)
                                      types (collect [_ v (ipairs completions)]
                                              (let [result (eval (.. "(type " v ")"))]
                                                (values v (or result.value
                                                              (let [e (?. result :error :message)]
                                                                (and e
                                                                     (if (string.find e "tried to reference a special form")
                                                                         "keyword"
                                                                         (string.find e "tried to reference a macro")
                                                                         "macro"
                                                                         "unknown")))))))]
                                  (json.encode {: op
                                                :symbol arg
                                                : completions
                                                : types} {})))
                  _
                  (if (?. ops op)
                      (let [output ((. ops op) arg)]
                        (dbg [:op op :output output])
                        (udp-out:send
                         (json.encode
                          (u.tbl.merge opts
                                       {: output}))))
                      (do (dbg "unknown op")
                          (udp-out:send
                           (json.encode
                            {:error {:type :unknow-op
                                     :message (.. "Reapl: '" op "' not supported.")}}))))))))
      (reaper.defer repl))))

(fn start-repl [{&as options : ports}]
  (setup-udp options)
  ((repl-fn)))
