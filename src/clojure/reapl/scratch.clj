(ns clojure.reapl.scratch
  (:require [reapl.interop :refer [>> <<]]))

(comment (<< (reaper.GetAppVersion))

         ;; I wrapped some parts of the API into fennel functions
         ;; It leaves in reaper-utils.fnl

         (>> (global ru (require :reaper-utils)))
         (>> (let [t (ru.take.get-active)]
               (ru.take.insert-note t {:pitch 57 :position 1 :duration 3})))

         (>> (global T (ru.take.get-active)))

         (<< (ru.take.grid.get T))
         (>> (ru.take.grid.set T 0.5))
         (>> (ru.take.grid.set T 2))

         (<< (ru.take.focus.get T))
         (<< (ru.take.focus.set T {:x 0 :y 60})))

(comment (>> (let [me (ru.midi-editor.get-active)
                   pc {:get (partial ru.midi-editor.pitch-cursor.get me)
                       :set (partial ru.midi-editor.pitch-cursor.set me)}]
               (pc.set 62)))

         (do :checks

             (<< (+ 4 5))
             (>> (+ 1 2))
             (<< (let [s "io,io"]
                   (s.gsub s "," " ")))
             (>> ((partial (fn [a b] (+ a b)) 1) 2))
             (<< {:a 1 :b 2}))

         (<< (global u (u.reload :utils)))

         (<< (global u (require :pb-utils))
             (global ru (u.reload :reaper-utils))
             (global T (ru.take.get-active))
             (global E (ru.midi-editor.get-active)))

         (<< (ru.take.get-active))
         (<< (ru.midi-editor.get-active))
         (<< (ru.take.clear T))
         (<< (ru.take.delete-selection T))
         (<< (ru.take.grid.get T))

         (<< (math.floor (reaper.MIDI_GetPPQPosFromProjQN (ru.take.get-active) 1)))
         (<< (/ (ru.take.grid.get T) 2))
         (<< (ru.take.grid.set T (/ 1 24)))
         (<< (let [(_ bpi) (reaper.GetProjectTimeSignature2 0)]
               bpi))
         (<< (ru.take.grid.set T 0.125))
         (<< (let [x (ru.take.grid.get T)]
               (ru.misc.log x)
               (ru.take.grid.set T (/ x 2))))
         (<< (let [x (* 2 (ru.take.grid.get T))]
               (ru.take.grid.set T x)))


         (<< (ru.take.time-selection.get T))
         (<< (ru.take.time-selection.set T 0 0))
         (<< (ru.take.time-selection.update T nil 2))
         (<< (ru.take.time-selection.update T :fw 1))
         (<< (ru.take.focused-note T))

         (<< (ru.take.cursor.get T))
         (<< (ru.take.cursor.set T 2))
         (<< (ru.take.cursor.update T 1))
         (<< (ru.take.cursor.update T -1))

         (<< (ru.midi-editor.pitch-cursor.get E))

         (>> (ru.take.insert-note T {:pitch 57 :position 1 :duration 3}))
         (<< package.path)
         (>> (let  [fnl (require "fennel")]
               (fnl.compile-string "(+ 1a 2)")))
         (>> (each [k _ (pairs (require "fennel"))]
                   (ru.misc.log k))))
