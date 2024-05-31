(local sok (require :reaper-socket))
(local bencode (require :bencode))
(local json (require :dkjson))
(local fnl (require :fennel))

(global u (require :pb-utils))
(global ru (require :reaper-utils))

(local {:misc {: log}} ru)

(local r reaper)

(local udp (assert (sok.udp)))
(local udp-out (assert (sok.udp)))

(fn setup-udp [in-port out-port]
  (udp:setoption "reuseaddr" true)
  (assert (udp:setsockname "127.0.0.1" in-port))
  (assert (udp-out:setpeername "127.0.0.1" out-port))
  (log (.. "setting udp sockets:\n"
           "in: localhost:" in-port
           "\nout: localhost:" out-port
           "\n\nWelcome to fennel repl !\n")))

(fn log-as-error [err]
  (log (.. "error:\n\n" (or err "nil"))))

(fn send-back-as-error [e]
  (udp-out:send (json.encode {:error e} {})))


(fn repl []
  (udp:settimeout 0.0001)
  (local m (udp:receive))
  (if m
      (let [{: code : compiled : no-return} (bencode.decode m)
            (compiled? ret) (pcall (fn [] (or compiled (fnl.compile-string code))))]
        (log (.. "__________\n\n>> " code "\n"))
        (if (not compiled?)
            (do (log-as-error ret)
                (or no-return
                    (send-back-as-error ret)))
            (let [(f err) (load ret)
                  (success ret) (pcall f)]
              (if success
                  (do (log ret)
                      (or no-return
                          (xpcall (fn [] (udp-out:send (json.encode ret {})))
                                  send-back-as-error)))
                  (do (log-as-error ret)
                      (or no-return
                          (send-back-as-error ret))))))))
  (reaper.defer repl))

(fn start-repl [{: socketPort : peerPort}]
  (setup-udp socketPort peerPort)
  (repl))
