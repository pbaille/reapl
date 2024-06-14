#!/usr/local/bin bb

(load-file "hydra-gen.clj")

(def reaper-hydra
  [:root nil
   {:color 'amaranth
    :doc "reaper hydra"
    :hydra/wrappers
    [(expr-wrapper
      (fn [e] (list 'reapl-mode_send-message :eval (str e))))]}
   '[:greet "g"
     (ru.misc.log "hey!")]
   '[:move "m"
     [:right "l"
      (ru.take.cursor.update (ru.take.get-active) 1)]
     [:left "h"
      (ru.take.cursor.update (ru.take.get-active) -1)]
     [:up "k"
      (let [me ru.midi-editor]
        (me.pitch-cursor.update (me.get-active) 1))]
     [:down"j"
      (let [me ru.midi-editor]
        (me.pitch-cursor.update (me.get-active) -1))]]])

(spit "reaper-hydra.el"
      (pretty-str (hydra-compile reaper-hydra)))
