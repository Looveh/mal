(ns step0-repl)

(defn READ [s]
  s)

(defn EVAL [s]
  s)

(defn PRINT [s]
  s)

(defn rep [s]
  (-> s READ EVAL PRINT))

(defn -main [& args]
  (println "args" args))
