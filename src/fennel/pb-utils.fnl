;; ------------------------------------------------------------
;; misc

(fn reload [p]
  "Reload the given package `p`."
  (tset package.loaded p nil)
  (require p))

(fn fold [x f xs]
  "Accumulate a result over the sequence `xs`, starting with `x` and applying the function `f`."
  (accumulate [x x _ e (ipairs xs)] (f x e)))


;; ------------------------------------------------------------
(local path {})

(fn path.pwd []
  "Get the current working directory."
  (: (io.popen "pwd") :read))

(set path.home "/Users/pierrebaille")
(set path.user (.. path.home "/Code/Lua"))

(fn path.relative [subpath]
  "Return the relative path by appending `subpath` to the current working directory."
  (.. (path.pwd) "/" subpath))


;; ------------------------------------------------------------
(local file {})

(fn file.slurp [path]
  "Read the contents of the file at `path`."
  (match (io.open path)
    f (let [content (f:read :*all)]
        (f:close)
        content)
    (nil err-msg) (print "Could not open file:" err-msg)))

(lambda file.spit [path content]
  "Write `content` to the file at `path`."
  (match (io.open path :w)
    f (do (f:write content)
          (f:close))
    (nil err-msg) (print "Could not open file:" err-msg)))


;; ------------------------------------------------------------
(local tbl {})

(fn tbl.matcher [m]
  "Return a matcher function for the table `m`."
  (case (type m)
    :function m
    :table (fn [t]
             (accumulate [ok true
                          km vm (pairs m)]
               (and ok
                    (case (. t km)
                      vt (case (type vm)
                           :function (vm vt)
                           _ (= vm vt))))))))

(fn tbl.getter [at]
  "Return a getter function for accessing elements in `at`."
  (case (type at)
    :string (fn [this] (?. this at))
    :table (fn [this]
             (accumulate [this this _ k (ipairs at)]
               (?. this k)))))

(fn tbl.match [t m]
  "Check if table `t` matches the pattern `m`."
  ((tbl.matcher m) t))

(fn tbl.get [t p]
  "Get the value at path `p` in table `t`."
  ((tbl.getter p) t))

(fn tbl.upd-at [t k u]
  "Update the key `k` in table `t` with the value `u`."
  (case (type u)
    :function (tset t k (u (. t k)))
    _ (tset t k u))
  t)

(fn tbl.upd [t u]
  "Update the table `t` with the updates in `u`."
  (case (type u)
    :table (each [k f (pairs u)]
             (tbl.upd-at t k f))
    :function (u t))
  t)

(fn tbl.merge [a b]
  "Merge table `b` into table `a`."
  (each [k v (pairs b)]
    (tset a k v))
  a)

(fn tbl.put [t k v]
  "Set key `k` in table `t` to value `v`."
  (tset t k v)
  t)

(fn tbl.rem [t k]
  "Remove key `k` from table `t`."
  (tset t k nil)
  t)

(fn tbl.keys [t]
  (icollect [k _ (pairs t)]
    k))

(fn tbl.vals [t]
  (icollect [_ v (pairs t)]
    v))

(fn tbl.= [t1 t2]
  (if (not (= (length t1) (length t2)))
      false
      (do (var equal true)
          (each [k v1 (pairs t1)]
            (let [v2 (. t2 k)]
              (if (or (and (= :table (type v1)) (not (table-equal v1 v2)))
                      (not (= v1 v2)))
                  (set equal false))))
          equal)))

;; ------------------------------------------------------------
(local seq {})

(fn seq.first [s]
  "Get the first element of the sequence `s`."
  (. s 1))

(fn seq.last [s]
  "Get the last element of the sequence `s`."
  (. s (length s)))

(fn seq.index-of [s v]
  "Get the index of value `v` in the sequence `s`."
  (var idx nil)
  (each [i x (ipairs s) &until idx]
    (if (= x v)
        (set idx i)))
  idx)

(fn seq.append [s x]
  "Append value `x` to the sequence `s`."
  (table.insert s x)
  s)

(fn seq.concat [s xs]
  "Concatenate sequences `s` and `xs`."
  (each [_ x (ipairs xs)]
    (seq.append s x))
  s)

(fn seq.keep [s f]
  "Keep elements of sequence `s` that satisfy the function `f`."
  (icollect [_ x (ipairs s)] (f x)))

(fn seq.filter [s f]
  "Filter elements of sequence `s` using the function `f`."
  (seq.keep s (fn [x] (if (f x) x))))

(fn seq.remove [s f]
  "Remove elements of sequence `s` that satisfy the function `f`."
  (seq.keep s (fn [x] (if (not (f x)) x))))

(fn seq.find [s f]
  "Find the first element in sequence `s` that satisfies the function `f`."
  (var found nil)
  (each [_ x (ipairs s) &until found]
    (if (f x)
        (set found x)))
  found)

(fn seq.sort [s key-fn compare-fn]
  "Sort sequence `s` using `key-fn` and `compare-fn`."
  (if (not key-fn) (table.sort s)
      (not compare-fn) (seq.sort-by s key-fn)
      (table.sort s (fn [a b] (compare-fn (key-fn a) (key-fn b)))))
  s)

(fn seq.sort-with [s f]
  "Sort sequence `s` using the function `f`."
  (table.sort s f)
  s)

(fn seq.sort-by [s key-fn]
  "Sort sequence `s` by the key function `key-fn`."
  (seq.sort-with s (fn [a b] (< (key-fn a) (key-fn b))))
  s)

(fn seq.reverse-sort-by [s key-fn]
  "Sort sequence `s` in reverse order by the key function `key-fn`."
  (seq.sort-with s (fn [a b] (> (key-fn a) (key-fn b))))
  s)

;; ------------------------------------------------------------
(local hof {})

(set hof.not #(not $))

(fn hof.k [x]
  "Return a function that always returns `x`."
  (fn [_] x))

(fn hof.inc [x]
  "Increment `x` by 1."
  (+ 1 x))

(fn hof.dec [x]
  "Decrement `x` by 1."
  (- 1 x))

(fn hof.adder [x]
  "Return a function that adds `x` to its argument."
  (fn [y] (+ x y)))

(fn hof.gt [x]
  "Return a function that returns true if its argument is greater than `x`."
  (fn [y] (> y x)))

(fn hof.lt [x]
  "Return a function that returns true if its argument is less than `x`."
  (fn [y] (< y x)))

(fn hof.gte [x]
  "Return a function that returns true if its argument is greater than or equal to `x`."
  (fn [y] (>= y x)))

(fn hof.lte [x]
  "Return a function that returns true if its argument is less than or equal to `x`."
  (fn [y] (<= y x)))


;; ------------------------------------------------------------
:tries

(macro scratch [...])

(scratch [:tries
          (relative-slurp "xp/first.fnl")
          (relative-spit "test.txt" "hello")
          (relative-spit "test.txt" )
          (relative-slurp "test.txt")]

         [:seq-tries
          (let [s [1 2 3 -4 8]]
            (seq.keep s (fn [x] (if (> x 0) (+ 1 x)))))
          (let [s [1 2 3 -4 8]]
            (fold 0 (fn [ret x] (if (> x 0) (+ ret x) ret)) s))
          (seq.first [1 2 3 4])
          (seq.last [1 2 3 4])
          (seq.append [1 2 3] 4)
          (seq.concat [1 2 3] [4 5 6])
          (seq.index-of [1 3 2 5 4 6 2] 2)
          (seq.sort-by [1 2 3 2 4 3] (fn [a] a))
          (seq.sort [1 2 3 4 3 2 5])
          (seq.sort [1 2 3 4 3 2 5] (fn [a] (* -1 a)))
          (seq.sort [1 2 3 4 3 2 5] (fn [a] (* -1 a)) (fn [a b] (> a b)))
          (seq.sort-with [1 2 3 2 4 3] (fn [a b] (> a b)))]

         [:tbl-tries
          (local m (table-matcher {:a 1
                                   :b (fn [x] (= :boolean (type x)))}))

          (tbl.= [:a :b] (tbl.keys {:a 1 :b 3}))
          (tbl.= [1 3] (tbl.vals {:a 1 :b 3}))

          (tbl.= {:a 1 :b 2}
                 {:a 1 :b 2})

          (tbl.upd {:a 1} {:a (hof.adder 3)})
          (tbl.upd {:a 1 :b 2}
                   {:b 67})

          (tbl.merge {:a 1 :b 2}
                     {:b 67 :c 90})

          (m {:a 1 :b true :x 4})
          (m {:a 1 :b 5})

          ((tbl.getter [:a :b]) {:x {:b 3}})
          ((tbl.getter [:a :b]) {:a {:b 3}})])



;; ------------------------------------------------------------
:export


{: path
 : file
 : tbl
 : hof
 : reload
 : fold
 : seq}
