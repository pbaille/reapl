(ns reapl.utils
  (:require [backtick :as bt]))

(defmacro template
  {:clj-kondo/ignore true}
  [x]
  `(bt/template ~x))
