(local {: tbl
        : clone
        : file
        : seq} (require :pb-utils))

(local ru (require :reaper-utils))

(ru.misc.log "LOAD ACTIONS")

(local reaper-actions
       {:move {:key "m"
               :children
               {:right {:key "l"
                        :fn #(ru.take.cursor.update (ru.take.get-active) 1)}
                :left {:key "h"
                       :fn #(ru.take.cursor.update (ru.take.get-active) -1)}
                :up {:key "k"
                     :fn #(let [me ru.midi-editor]
                            (me.pitch-cursor.update (me.get-active) 1))}
                :down {:key "j"
                       :fn #(let [me ru.midi-editor]
                              (me.pitch-cursor.update (me.get-active) -1))}}}

        :grid {:key "g"
               :children {:quarters {:key "n"
                                     :fn #(ru.take.grid.set T 1)}
                          :mul3 {:key "J"
                                 :fn #(let [T (ru.take.get-active)]
                                        (ru.take.grid.set T (* 3 (ru.take.grid.get T))))}
                          :mul2 {:key "j"
                                 :fn #(let [T (ru.take.get-active)]
                                        (ru.take.grid.set T (* 2 (ru.take.grid.get T))))}
                          :div2 {:key "k"
                                 :fn #(let [T (ru.take.get-active)]
                                        (ru.take.grid.set T (/ (ru.take.grid.get T) 2)))}
                          :div3 {:key "K"
                                 :fn #(let [T (ru.take.get-active)]
                                        (ru.take.grid.set T (/ (ru.take.grid.get T) 3)))}}}

        :time-selection {:key "t"
                         :children {:move-fw {:key "l"
                                              :fn #(ru.take.time-selection.update (ru.take.get-active) nil 1)}
                                    :move-bw {:key "h"
                                              :fn #(ru.take.time-selection.update (ru.take.get-active) nil -1)}
                                    :shrink-fw {:key "H"
                                                :fn #(let [t ru.take
                                                           T (t.get-active)]
                                                       (t.time-selection.update T :fw -1)
                                                       (t.cursor.set T (. (t.time-selection.get T) :end)))}
                                    :shrink-bw {:key "C-l"
                                                :fn #(let [t ru.take
                                                           T (t.get-active)]
                                                       (t.time-selection.update T :bw 1)
                                                       (t.cursor.set T (. (t.time-selection.get T) :start)))}
                                    :grow-fw {:key "L"
                                              :fn #(let [t ru.take
                                                         T (t.get-active)]
                                                     (t.time-selection.update T :fw 1)
                                                     (t.cursor.set T (. (t.time-selection.get T) :end)))}
                                    :grow-bw {:key "C-h"
                                              :fn #(let [t ru.take
                                                         T (t.get-active)]
                                                     (t.time-selection.update T :bw -1)
                                                     (t.cursor.set T (. (t.time-selection.get T) :start)))}
                                    :clear {:key "x"
                                            :fn #(ru.take.time-selection.set (ru.take.get-active) 0 0)}}}

        :note {:key "n"
               :children {:insert {:key "i"
                                   :fn #(let [t ru.take
                                              T (t.get-active)
                                              focus (t.focus.get T)
                                              grid (t.grid.get-ppq T)]
                                          (t.insert-note T {:start-position focus.x
                                                            :end-position (+ focus.x grid)
                                                            :pitch focus.y}))}
                          :next {:key "f"
                                 :fn #(let [T (ru.take.get-active)]
                                        (ru.take.focus.next-note T))}
                          :previous {:key "b"
                                     :fn #(let [T (ru.take.get-active)]
                                            (ru.take.focus.previous-note T))}

                          :toggle-selection {:key "t"
                                             :fn #(let [T (ru.take.get-active)]
                                                    (ru.take.set-note T (u.tbl.upd (ru.take.focused-note T)
                                                                                   {:selected u.hof.not})))}

                          :channel {:key "c"
                                    :children {:up {:key "k"
                                                    :fn #(let [T (ru.take.get-active)]
                                                           (ru.take.set-note T (u.tbl.upd (ru.take.focused-note T)
                                                                                          {:channel (fn [c] (% (+ 1 c) 16))})))}
                                               :down {:key "j"
                                                      :fn #(let [T (ru.take.get-active)]
                                                             (ru.take.set-note T (u.tbl.upd (ru.take.focused-note T)
                                                                                            {:channel (fn [c] (% (- c 1) 16))})))}}}

                          :velocity {:key "v"
                                     :children {:up {:key "k"
                                                     :fn #(let [T (ru.take.get-active)]
                                                            (ru.take.set-note T (u.tbl.upd (ru.take.focused-note T)
                                                                                           {:velocity (fn [v] (math.min 127 (+ 10 v)))})))}
                                                :down {:key "j"
                                                       :fn #(let [T (ru.take.get-active)]
                                                              (ru.take.set-note T (u.tbl.upd (ru.take.focused-note T)
                                                                                             {:velocity (fn [v] (math.min 127 (- v 10)))})))}}}}}})

(fn action-path [p]
  (-?> (tbl.path p)
       (seq.interpose :children)))

(fn do-action [p]
  "Retrieve the fn at given path and call it"
  (let [fn-path (-?> (action-path p)
                     (seq.append :fn))]
    (case (tbl.get reaper-actions fn-path)
      f (f))))

(fn str-join [s x]
  (var ret "")
  (each [i v (ipairs s)]
    (if (> i 1)
        (set ret (.. ret x v))
        (set ret v)))
  ret)

(fn get-binding-tree []
  (tbl.indexed-prewalk (clone reaper-actions)
                       (fn [at node]
                         (if (= :table (type node))
                             (let [f (. node :fn)]
                               (when f
                                 (tset node :fn (str-join (seq.take-nth at 2) "-")))
                               node)
                             node))))

(fn make-action-dispatcher []
  (var dispatch {})
  (tbl.indexed-prewalk (clone reaper-actions)
                       (fn [at node]
                         (if (= :table (type node))
                             (let [f (. node :fn)]
                               (when f
                                 (tset dispatch (str-join (seq.take-nth at 2) "-") f))
                               node)
                             node)))
  dispatch)

(fn file-exists? [path]
  (let [f (io.open path :r)]
    (if f (do (f:close) true))))

(fn create-actions-dir [compilation-path dispatch]
  (let [actions-path (.. compilation-path "/actions")
        action-name->script (fn [name]
                              (.. "package.path = \"" compilation-path "/?.lua;\" .. package.path\n"
                                  "if not reapl_actions then reapl_actions = require(\"reaper-actions\")\n end"
                                  "reapl_actions.dispatch[\"" name "\"]()"))]
    (if (not (os.execute (.. "ls -d " actions-path)))
        (os.execute (.. "mkdir " actions-path)))
    (if (or (not (file-exists? (.. compilation-path "/reaper-utils.lua")))
            (not (file-exists? (.. compilation-path "/reaper-actions.lua"))))
        (error "reaper-utils.lua and reaper-acttions.lua should be in the given compilation path"))

    (var codes {})
    (each [name _ (pairs dispatch)]
      (let [filepath (.. actions-path "/pbaille_" name ".lua")]
        (file.spit filepath
                   (action-name->script name))
        (tset codes name (reaper.AddRemoveReaScript true 0 filepath true))))
    codes))

(comment
 ((?. actions :move :children :left :fn))
 (action-path :move.left)
 (actions.do-action :move.left)
 (do-action :move.right)
 (do-action :move.up)
 (do-action nil))

{:tree reaper-actions
 : do-action
 : get-binding-tree
 :dispatch (make-action-dispatcher)
 : create-actions-dir}
