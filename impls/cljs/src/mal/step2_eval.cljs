(ns mal.step2-eval
  (:require [mal.reader :as reader :refer [->form]]
            ["readline" :as readline]))

(def rl
  (.createInterface readline #js {:input js/process.stdin
                                  :output js/process.stdout}))

(def repl-env
  {"+" (fn [a b]
         (->form "number" (+ a.value b.value)))
   "-" (fn [a b]
         (->form "number" (- a.value b.value)))
   "*" (fn [a b]
         (->form "number" (* a.value b.value)))
   "/" (fn [a b]
         (->form "number" (/ a.value b.value)))})

(declare eval-ast)

(defn READ [s]
  (reader/read-str' s))

(defn EVAL [ast env]
  (cond
    (not= "list" ast.type)
    (eval-ast ast env)

    (empty? ast.value)
    ast

    :else
    (let [res (eval-ast ast env)
          operator (aget res.value 0)
          symbol (when (= "symbol" operator.type)
                   (get env operator.value))
          operands (.slice res.value 1)]
      (when-not symbol
        (throw (str "Symbol not found: '" res.value "'")))
      (apply symbol operands))))

(defn eval-ast [ast env]
  (let [type ast.type
        value ast.value]
    (cond
      (= "list" type)
      (reader/->form "list" (let [res #js []]
                              (doseq [e value]
                                (.push res (EVAL e env)))
                              res))

      (= "vec" type)
      (reader/->form "vec" (let [res #js []]
                             (doseq [e value]
                               (.push res (EVAL e env)))
                             res))

      (= "hash-map" type)
      (reader/->form "hash-map" (let [res #js []]
                                  (doseq [e value]
                                    (.push res (EVAL e env)))
                                  res))

      :else
      ast)))

(defn PRINT [form]
  (println (reader/pr-str' form)))

(defn rep [s]
  (-> s
      (READ)
      (EVAL repl-env)
      (PRINT)))

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

