(local utils (require :fennel.utils))
(local parser (require :fennel.parser))
(local compiler (require :fennel.compiler))
(local specials (require :fennel.specials))
(local view (require :fennel.view))

(fn complete [{&as opts : env : scope} text]
  (let [max-items 2000 ; to stop explosion on too mny items
        seen {}
        matches []
        input-fragment (text:gsub ".*[%s)(]+" "")]
    (var stop-looking? false)

    (fn add-partials [input tbl prefix] ; add partial key matches in tbl
      ;; When matching on global env or repl locals, iterate *manglings* to include nils
      (local scope-first? (or (= tbl env) (= tbl env.___replLocals___)))
      (icollect [k is-mangled (utils.allpairs (if scope-first? scope.manglings tbl))
                 :into matches :until (<= max-items (length matches))]
        (let [lookup-k (if scope-first? is-mangled k)]
          (when (and (= (type k) :string) (= input (k:sub 0 (length input)))
                     ;; manglings iterated for globals & locals, but should only match once
                     (not (. seen k))
                     ;; only match known  functions when we encounter a method call
                     (or (not= ":" (prefix:sub -1)) (= :function (type (. tbl lookup-k)))))
            (tset seen k true)
            (.. prefix k)))))

    (fn descend [input tbl prefix add-matches method?]
      (let [splitter (if method? "^([^:]+):(.*)" "^([^.]+)%.(.*)")
            (head tail) (input:match splitter)
            raw-head (or (. scope.manglings head) head)]
        (when (= (type (. tbl raw-head)) :table)
          (set stop-looking? true)
          (if method?
              (add-partials tail (. tbl raw-head) (.. prefix head ":"))
              (add-matches tail (. tbl raw-head) (.. prefix head))))))

    (fn add-matches [input tbl prefix]
      (let [prefix (if prefix (.. prefix ".") "")]
        (if (and (not (input:find "%.")) (input:find ":")) ; found a method call
            (descend input tbl prefix add-matches true)
            (not (input:find "%.")) ; done descending; add matches
            (add-partials input tbl prefix)
            (descend input tbl prefix add-matches false))))

    (each [_ source (ipairs [scope.specials scope.macros
                             (or env.___replLocals___ []) env env._G])
           :until stop-looking?]
      (add-matches input-fragment source))
    matches))

(fn splice-save-locals [{: env : scope} lua-source]
  (let [saves (icollect [name (pairs env.___replLocals___)]
                (: "local %s = ___replLocals___[%q]"
                 :format (or (. scope.manglings name) name) name))
        binds (icollect [raw name (pairs scope.manglings)]
                (when (not (. scope.gensyms name))
                  (: "___replLocals___[%q] = %s"
                   :format raw name)))
        gap (if (lua-source:find "\n") "\n" " ")]
    (.. (if (next saves) (.. (table.concat saves " ") gap) "")
        (case (lua-source:match "^(.*)[\n ](return .*)$")
          (body return) (.. body gap (table.concat binds " ") gap return)
          _ lua-source))))

(fn resolve [{: env : scope} identifier]
  (let [e (setmetatable {} {:__index #(or (. env.___replLocals___
                                             (. scope.unmanglings $2))
                                          (. env $2))})]
    (case-try (pcall compiler.compile-string (tostring identifier) {: scope})
      (true code) (pcall (specials.load-code code e))
      (true val) val
      (catch _ nil))))

(fn doc [{&as opts : env : scope} name]
  (let [path (or (utils.multi-sym? name) [name])
        (ok? target) (pcall #(or (utils.get-in scope.specials path)
                                 (utils.get-in scope.macros path)
                                 (resolve opts name)))]
    (if ok?
        {:value (specials.doc target name)}
        {:error {:type :repl
                 :message (.. "Could not find " name " for docs.")}})))

(fn info [{&as opts : env : scope} name]
  (-?> name (resolve opts) (debug.getinfo)))

(fn find [{&as opts : env : scope} name]
  (case (info opts name)
    {:what "Lua" : source :linedefined line :short_src src}
    (let [fnlsrc (?. compiler.sourcemap source line 2)]
      {:value (string.format "%s:%s" src (or fnlsrc line))})
    nil {:error {:type :repl
                 :message "Unknown value"}}
    _ {:error {:type :repl
               :message "No source info"}}))

(comment (find {:env (specials.wrap-env _G)
                :scope (compiler.make-scope)}
               "pairs")

         (global foo 43)
         (debug.getinfo (resolve {:env (specials.wrap-env _G)
                                  :scope (compiler.make-scope)}
                                 "foo"))

         (utils.sym? "iop"))

(fn repl []
  (let [scope (compiler.make-scope)
        env (specials.wrap-env _G)
        useMetadata true
        opts {: env : scope : useMetadata}]
    (set opts.env.___replLocals___
         (setmetatable {}
                       {:__newindex (fn [t k v]
                                      (when (. opts.scope.manglings k) (rawset t k v)))}))
    {:complete (partial complete opts)
     :doc (partial doc opts)
     :eval (fn [code-str]
             (let [(ok parser-not-eof? form) (pcall (parser.parser code-str))]
               (if (not ok)
                   {:error {:type :parse :data [parser-not-eof? form]}}
                   (case-try (pcall compiler.compile form
                                    (doto opts (tset :source code-str)))
                     (true src) (let [src (splice-save-locals opts src)
                                      (ok? f) (pcall specials.load-code src opts.env)]
                                  (if ok?
                                      (let [value (f)]
                                        {: value :value-str (view value)})
                                      {:error {:type :load :message f}}))
                     (catch
                      (false msg) {:error {:type :compile :message msg}})))))}))

(comment
 (let [{: eval : complete} (repl)]
   ((. (eval "(do (local x 3) (+ x x))")
       :return)))

 (let [{: eval : complete} (repl)]
   ((. (eval "(global x 3)")
       :return))
   ((. (eval "(+ x x)")
       :return)))

 (?. {:foo {:bar 2}} :foo :bar))

(repl)
