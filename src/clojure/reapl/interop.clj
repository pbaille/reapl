(ns reapl.interop
  (:refer-clojure :exclude [send])
  (:require [reapl.socket :as socket]
            [bencode.core :as bc]
            [backtick :as bt]
            [clojure.core.async :as async]
            [clojure.data.json :as json]
            [clojure.string :as str]))

(def LOCALHOST "127.0.0.1")
(def REAPER_IN_PORT 9999)
(def REAPER_OUT_PORT 9997)

(defonce reaper-input-chan
  (socket/udp-chan REAPER_OUT_PORT))

(defn reset-reaper-input-chan! []
  (alter-var-root #'reaper-input-chan
                  (fn [{:keys [socket channel]}]
                    (.close socket)
                    (async/close! channel)
                    (socket/udp-chan REAPER_OUT_PORT))))

(defn encode [data]
  (-> (doto (java.io.ByteArrayOutputStream.)
        (bc/write-bencode data))
      .toString))

(defn message [code]
  {:code (str/replace (str code) "," " ")})

(defn send [code]
  (socket/send-message LOCALHOST REAPER_IN_PORT
                       (encode (assoc (message code)
                                      :no-return "yes"))))

(defmacro >>
  {:clj-kondo/ignore true}
  ([code]
   `(send (bt/template ~code)))
  ([x & xs]
   `(>> (do ~x ~@xs))))

(defn get-response! [timeout]
  (let [timeout-chan (async/timeout timeout)
        [v p] (async/alts!! [(:buffer reaper-input-chan) timeout-chan])]
    (if (= p timeout-chan)
      ::reaper-timeout
      (json/read-str v :key-fn keyword))))

(defn ask [code]
  (socket/send-message LOCALHOST REAPER_IN_PORT
                       (encode (message code)))
  (get-response! 2000))

(defmacro <<
  {:clj-kondo/ignore true}
  ([code]
   `(ask (bt/template ~code)))
  ([x & xs]
   `(<< (do ~x ~@xs))))
