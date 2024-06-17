(local sok (require :reaper-socket))
(local json (require :dkjson))

(global fennel (require :fennel))
(global u (require :pb-utils))
(global ru (require :reaper-utils))
(global reascript-doc (require :reascript-doc))
(global actions (require :reaper-actions))
(global r reaper)

(fn dbg [...]
    (if debug
        (each [_ x (ipairs [...])]
          (ru.misc.log (if (= :string (type x))
                           x
                           (fennel.view x))))))

(var debug true)

(local input-socket (assert (sok.udp)))
(local output-socket (assert (sok.udp)))
(local action-socket (assert (sok.udp)))


(fn setup-udp [{:ports {: input : output : action}}]
  (input-socket:setoption "reuseaddr" true)
  (input-socket:setoption "send-buffer-size" 32000)
  (action-socket:setoption "reuseaddr" true)
  (action-socket:settimeout 0.0001)
  (assert (action-socket:setsockname "127.0.0.1" action))
  (assert (input-socket:setsockname "127.0.0.1" input))
  (assert (output-socket:setpeername "127.0.0.1" output))
  (ru.misc.log (.. "setting udp sockets:\n"
                   "in: localhost:" input
                   "\nout: localhost:" output
                   "\naction: localhost:" action
                   "\n\nReapl server running !\n")))

(fn encode [data]
  (fn deep-encode-fn [t]
    (case (type t)
      :function "#<function>"
      :table
      (let [result {}]
        (each [k v (pairs t)]
          (tset result k (deep-encode-fn v)))
        result)
      _ t))
  (json.encode (deep-encode-fn data)
               {}))

(fn respond [opts output]
  (let [res (u.tbl.merge opts
                         {: output})]
    (dbg [:out res])
    (output-socket:send (encode res))))

(fn error-handler [opts error-type]
  (fn [e]
    (respond opts {:error {:type error-type :message e}})))

(fn fennel-type-error->kind [e]
  (and e
       (if (string.find e "tried to reference a special form")
           "keyword"
           (string.find e "tried to reference a macro")
           "macro"
           "unknown")))

(fn action-step [port]
  (fn []
    (let [m (action-socket:receive)]
      (when m
        (dbg ["got action " m])
        (let [f (?. actions.dispatch m)]
          (if f (f)))))))

(fn repl-step []
  (let [{: complete : eval &as ops} (require :simple-repl)
        {: listen} (require :udp-utils)]
    (listen input-socket
            (fn [{&as opts : op : data}]

              (dbg [:in opts])
              (case op

                :do
                (xpcall (fn [] (eval data))
                        (error-handler opts "do"))

                :eval
                (xpcall (fn [] (respond opts (eval data)))
                        (error-handler opts "eval"))

                :complete
                (let [completions (complete data)
                      types (collect [_ v (ipairs completions)]
                              (let [result (eval (.. "(type " v ")"))]
                                (values v (or result.value
                                              (fennel-type-error->kind (?. result :error :message))))))]
                  (respond opts {: completions
                                 : types}))
                _
                (if (?. ops op)
                    (respond opts ((. ops op) data))
                    (respond opts {:error {:type :unknow-op
                                           :message (.. "Reapl: '" op "' not supported.")}})))))))

(fn start-repl [{&as options : ports}]
  (setup-udp options)
  (let [action-step (action-step (. ports :action))
        repl-step (repl-step)
        loop (fn rec []
               (action-step)
               (repl-step)
               (reaper.defer rec))]
    (loop)))
