(ns mal.step1-read-print
  (:require [mal.reader :as reader]
            ["readline" :as readline]))

(def rl
  (.createInterface readline #js {:input js/process.stdin
                                  :output js/process.stdout}))

(defn READ [s]
  (reader/read-str' s))

(defn EVAL [form]
  form)

(defn PRINT [form]
  (println (reader/pr-str' form)))

(defn rep [s]
  (-> s READ EVAL PRINT))

(defn main-loop []
  (let [f (fn [s]
            (try
              (rep s)
              (catch :default e
                (println e)))
            (main-loop))]
    (.question rl "user> " f)))

(defn -main [& _args]
  (main-loop))
