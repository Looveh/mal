(ns mal.step1-read-print
  (:require ["readline" :as readline]))

(def rl
  (.createInterface readline #js {:input js/process.stdin
                                  :output js/process.stdout}))

(defn READ [s]
  s)

(defn EVAL [s]
  s)

(defn PRINT [s]
  (println s))

(defn rep [s]
  (-> s READ EVAL PRINT))

(defn main-loop []
  (.question rl "user> " (fn [s]
                          (rep s)
                          (main-loop))))

(defn -main [& _args]
  (main-loop))
