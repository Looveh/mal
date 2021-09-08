(ns mal.reader
  (:require [clojure.string]))

(declare read-form)

(defn ->form [type value]
  #js {:type type :value value})

(defn read-list [{:keys [peek next] :as reader} opener]
  (let [type (condp = opener
               "(" "list"
               "[" "vec"
               "{" "hash-map")
        closer (condp = opener
                 "(" ")"
                 "[" "]"
                 "{" "}")
        ret (->form type #js [])]
    (loop []
      (cond
        (= closer (peek))
        (next)

        (= type "hash-map")
        (let [k (read-form reader)
              v (read-form reader)]
          (when (or (= k closer) (= v closer))
            (throw "has-map must contain even number of forms"))
          (.push ret.value k)
          (.push ret.value v)
          (recur))

        :else
        (do
          (.push ret.value (read-form reader))
          (recur))))
    ret))

(defn read-atom [{:keys [next peek] :as reader}]
  (println (peek))
  (let [s (next)]
    (cond
      (= "@" s)
      (->form "list" #js [(->form "symbol" "deref")
                          (read-form reader)])

      (= "'" s)
      (->form "list" #js [(->form "symbol" "quote")
                          (read-form reader)])

      (= "~" s)
      (->form "list" #js [(->form "symbol" "unquote")
                          (read-form reader)])

      (= "`" s)
      (->form "list" #js [(->form "symbol" "quasiquote")
                          (read-form reader)])

      (= "~@" s)
      (->form "list" #js [(->form "symbol" "splice-unquote")
                          (read-form reader)])

      (= "^" s)
      (let [m (read-form reader)
            v (read-form reader)]
        (->form "list" #js [(->form "symbol" "with-meta") v m]))

      (.startsWith s "\"")
      (if (or (= "\"" s)
              (not (.endsWith s "\""))
              (.endsWith s "\\\""))
        (throw "EOF")
        (->form "string" s))

      (= "nil" s)
      (->form "nil" nil)

      (= "true" s)
      (->form "bool" true)

      (= "false" s)
      (->form "bool" false)

      (re-matches #"^(-)?\d+(\.\d+)?$" s)
      (->form "number" (js/parseFloat s))

      :else
      (->form "symbol" s))))

(defn read-form [{:keys [peek next] :as reader}]
  (let [v (peek)]
    (if (re-matches #"[\[\(\{]" v)
      (do (next) (read-list reader v))
      (read-atom reader))))

(defn ->reader [tokens]
  (let [pos (atom 0)
        eof (fn []
              (throw "EOF"))]
    {:peek (fn []
             (let [v (get tokens @pos)]
               (when-not v
                 (eof))
               v))
     :next (fn []
             (swap! pos inc)
             (let [v (get tokens (dec @pos))]
               (when-not v
                 (eof))
               v))}))

(defn tokenize [s]
  (let [re #"[\s,]*(~@|[\[\]{}()'`~^@]|\"(?:\\.|[^\\\"])*\"?|\;\.*|[^\s\[\]{}('\"\`,\;)]*)"]
    (->> (re-seq re s)
         (map second)
         (filter identity)
         (filter #(not= "" %))
         (vec))))

(defn read-str' [s]
  (let [tokens (tokenize s)
        reader (->reader tokens)]
    (read-form reader)))

(defn pr-str' [form]
  (let [type form.type
        value form.value]
    (condp = type
      "symbol" (str value)
      "number" (str value)
      "list" (str "(" (clojure.string/join " " (map pr-str' value)) ")")
      "vec" (str "[" (clojure.string/join " " (map pr-str' value)) "]")
      "hash-map" (str "{" (clojure.string/join " " (map pr-str' value)) "}")
      "quoted" (str value)
      "string" value
      "nil" "nil"
      "bool" (str value)
      (throw (str "Unknown type '" type "'")))))
