(local json (require :dkjson))
(local sok (require :socket))

(local BEG_DEL "<UDP|")
(local END_DEL "|UDP>")

(fn last-chunk? [s]
  (= END_DEL
     (s:sub (- (# END_DEL)))))

(fn first-chunk? [s]
  (= BEG_DEL
     (s:sub 1 (# BEG_DEL))))

(fn chunks-to-string [xs]
  (var ret "")
  (each [i v (ipairs xs)]
    (set ret (.. ret v)))
  (set ret (ret:sub (+ 1 (# BEG_DEL)) (- (+ 1 (# END_DEL)))))
  ret)

(fn decode-chunks [xs on-success on-error]
  (let [str (chunks-to-string xs)]
    (case (json.decode str)
      (ret _ nil) (on-success ret)
      (nil _ e) (on-error {:type :encode
                           :error e
                           :message str}))))

(fn listen [socket on-success on-error]
  (var input nil)
  (socket:settimeout 0.0001)
  (fn loop []
    (let [chunk (socket:receive)]
      (when chunk
        (if (first-chunk? chunk)
            (set input [chunk])
            (if input
                (table.insert input chunk)))
        (if (and input (last-chunk? chunk))
            (decode-chunks input on-success on-error))
        (loop)))))

(comment
 (do
   (local sok1 (assert (sok.udp)))
   (local sok2 (assert (sok.udp)))
   (sok1:setoption "reuseaddr" true)
   (sok2:setoption "reuseaddr" true)
   (assert (sok1:setsockname "127.0.0.1" "55551"))
   (assert (sok2:setpeername "127.0.0.1" "8083"))
   (local view (require :fennel.view))
   (local co (listen sok1
                     (fn [m] (print (view {:got m})))
                     (fn [e] (print (view {:err e} )))))
   (local go (listen sok1
                     (fn [m] (print (view {:got m})))
                     (fn [e] (print (view {:err e} )))))
   (go)
   (sok2:send "<UDP|{\"io\": [")
   (sok2:send "1,2]}|UDP>")
   (sok2:send "|UDP>")
   (sok2:send "<UDP|{\"hey\": 3|UDP>")

   (coroutine.resume co)
   (comment (+ 1 2)
            (sok1:settimeout 0.0001)
            (sok1:receive)
            (coroutine.close co)
            (let [(ret _ e) (json.decode "{\"io\": [1]}")]
              [ret e])
            (let [(ret _ e) (json.decode "{\"io\": [1]")]
              [ret e]))))
