(ns reader
  (:require [cljs.test :refer [are is]]))

#_(defn _tokenize
  {:test
   (fn []
     (are [a b] (= a (js->clj b))
       ["+"] (_tokenize "+")
       ["-" "+"] (_tokenize "- +")
       ["-" "+"] (_tokenize " -  + ")
       [["+" "1" "2"]] (_tokenize "(+ 1 2)")
       [["+" "1" ["-" "32" "14"]]] (_tokenize " (\t+ 1\n( - 32 14 ))")
       [[".eat" "cow" "'grass"]] (_tokenize "(.eat cow 'grass)")
       [[":a" "b" "3"]] (_tokenize "(:a b 3)")))}
  [s]
  (let [token-c-re #"[a-zA-Z0-9\-\_\*\!\?\=\<\>\.\+\'\:]"
        ast #js []
        path #js [0]
        token (volatile! nil)
        add! (fn []
               (when @token
                 (cond
                   (= 1 (count path))
                   (apply aset ast (.concat path @token))

                   (= 0 (last path))
                   (do
                     (.pop path)
                     (apply aset ast (.concat path #js [#js []]))
                     (.push path 0)
                     (apply aset ast (.concat path @token)))

                   :else
                   (apply aset ast (.concat path @token)))
                 (.push path (inc (.pop path)))
                 (vreset! token nil)))]
    (doseq [c s]
      (cond
        (re-matches token-c-re c) (vreset! token (str @token c))
        (re-matches #"\s" c) (add!)
        (= "(" c) (.push path 0)
        (= ")" c) (do
                    (add!)
                    (.pop path))))
    (add!)
    ast))

(declare read-form)

(defn read-list [{:keys [peek next] :as reader}]
  (loop [the-list []]
    (if (= ")" (peek))
      (do (next) the-list)
      (recur (conj the-list (read-form reader))))))

(defn read-atom [{:keys [next]}]
  (let [v (next)]
    (cond
      (re-matches #"\d+" v) (js/parseInt v)
      :else v)))

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

(defn tokenize' [s]
  (let [re #"[\s,]*(~@|[\[\]{}()'`~^@]|\"(?:\\.|[^\\\"])*\"?|\;\.*|[^\s\[\]{}('\"\`,\;)]*)"]
    (->> (re-seq re s)
         (map second)
         (filter #(not= "" %))
         (vec))))

(defn read-str' [s]
  (let [tokens (tokenize' s)
        reader (->reader tokens)]
    (read-form reader)))

(comment 
  (read-str' "+")
  (tokenize "(+ a b)")
  )
