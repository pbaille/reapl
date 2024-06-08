#!/usr/local/bin bb

(require '[cheshire.core :as json]
         '[babashka.fs :as fs]
         '[clojure.string :as str])

(defn extend-function-infos
  [{:keys [namespace name params returns description] :as obj}]
  (let [params (or params [])
        returns (or returns [])
        param-names (mapv (fn [{:keys [name optional]}]
                            (str (when optional "?") name))
                          params)
        return-type (mapv :type returns)
        form-string (format "(%s.%s %s) :: %s"
                            namespace
                            name
                            (str/join " " param-names)
                            return-type)
        param-types-string (str/join "\n" (map (fn [{:keys [name type optional]}]
                                                 (str name
                                                      " :: "
                                                      type
                                                      (when optional " (optional)")))
                                               params))]
    (assoc obj
           :form form-string
           :doc (str form-string "\n\n" param-types-string "\n\n" description))))

(defn read-json-file [file]
  (json/parse-string (slurp file) true))

(defn data->lua [data]
  (str (str/join "\n"
                 (concat '[(local fnl (require :fennel))
                           (fn add-meta [f arglist docstring]
                             (if f
                               (fnl.metadata:setall f
                                                    :fnl/arglist arglist
                                                    :fnl/docstring docstring)))]
                         (mapv (fn [spec]
                                 (list 'add-meta
                                       (symbol (str "reaper."(:name spec)))
                                       (mapv :name (:params spec))
                                       (:description spec)))
                               data)))
       "\n{}"))

(let [[input-file output-file] *command-line-args*]
  (let [data (read-json-file input-file)]
    (spit output-file (data->lua data))))
