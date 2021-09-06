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
          operands (.slice res.value 1)]
      (apply operator operands))))

(defn eval-ast [ast env]
  (let [type ast.type
        value ast.value]
    (cond
      (= "symbol" type)
      (if-let [symbol (get env value)]
        symbol
        (throw (str "Symbol not found: '" value "'")))

      (= "list" type)
      (reader/->form "list" (let [res #js []]
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
  (.question rl "user> " (fn [s]
                           (rep s)
                           (main-loop))))

(defn -main [& _args]
  (main-loop))

