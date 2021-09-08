(ns mal.reader
  (:require [clojure.string]))

(declare read-form)

(defn ->form [type value]
  #js {:type type :value value})

(defn read-list [{:keys [peek next] :as reader}]
  (let [ret (->form "list" #js [])]
    (loop []
      (cond
        (= ")" (peek))
        (next)

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

      (= "^" s)
      (let [m (read-form reader)
            v (read-form reader)]
        (->form "list" #js [(->form "symbol" "with-meta") v m]))
      
      (re-matches #"(-)?\d+(\.\d+)?" s)
      (->form "number" (js/parseFloat s))

      :else
      (->form "symbol" s))))

(defn read-form [{:keys [peek next] :as reader}]
  (if (= "(" (peek))
    (do (next) (read-list reader))
    (read-atom reader)))

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
      "quoted" (str value)
      (throw (str "Unknown type '" type "'")))))

(comment
  (pr-str' (read-str' "^{:a 1} (+ 1 2)")))
