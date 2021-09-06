(ns mal.reader
  (:require [clojure.string]))

(declare read-form)

(defn ->form [type value]
  #js {:type type :value value})

(defn read-list [{:keys [peek next] :as reader}]
  (let [ret (->form "list" #js [])]
    (loop []
      (cond
        (nil? (peek))
        (throw "EOF")

        (= ")" (peek))
        (next)

        :else
        (do
          (.push ret.value (read-form reader))
          (recur))))
    ret))

(defn read-atom [{:keys [next]}]
  (let [v (next)]
    (cond
      (re-matches #"\d+(\.\d+)?" v)
      (->form "number" (js/parseFloat v))

      :else
      (->form "symbol" v))))

(defn read-form [{:keys [peek next] :as reader}]
  (if (= "(" (peek))
    (do (next) (read-list reader))
    (read-atom reader)))

(defn ->reader [tokens]
  (let [pos (atom 0)]
    {:peek (fn []
             (get tokens @pos))
     :next (fn []
             (swap! pos inc)
             (get tokens (dec @pos)))}))

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
      (throw (str "Unknown type '" type "'")))))
