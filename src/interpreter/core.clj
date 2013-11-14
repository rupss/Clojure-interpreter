(ns interpreter.core
  (:gen-class)
  (:require [clojure.string :as str]))

(def operator-map {"+" + "-" - "*" * "/" /})
(def bool-map {"true" true "false" false})
(def builtin-map {
  "map" map
  "filter" filter
  "conj" conj
  "cons" cons
  "first" first
  "last" last
  "not" not
  "rest" rest})

(defn is-vector?
  [item]
  (= clojure.lang.PersistentVector (type item)))

;; TAGGING

(defn str-literal
  [token]
  (if (and (= (first token) (char 34)) (= (last token) (char 34)))
    token))

(defn num-literal
  [token]
  (try
    (Integer/parseInt token)
    (catch Exception e
      nil)))
  
(defn tag
  [token]
  (let [operator (operator-map token)
        str-literal (str-literal token)
        num-literal (num-literal token)
        bool (bool-map token)
        builtin (builtin-map token)]
  (cond
    (not (nil? operator)) {:type :operator :value operator}
    (not (nil? str-literal)) {:type :literal :value str-literal}
    (not (nil? num-literal)) {:type :literal :value num-literal}
    (not (nil? bool)) {:type :bool :value bool}
    (not (nil? builtin)) {:type :builtin :value builtin}
    :else {:type :identifier :value token})))

;; TOKENIZING

(defn is-list-item?
  [item]
  ; (println "#### IS-LIST-ITEM? #####")
  ; (println item)
  ; (println (type item))
  (and (= clojure.lang.PersistentArrayMap (type item)) (= (item :type) :list)))

(defn get-struct-type
  [curr-struct tagged-item]
  (cond
   (is-vector? curr-struct) :vector
   (is-list-item? curr-struct) :list))

(defmulti insert-into-top get-struct-type)

(defmethod insert-into-top :vector
  [curr-struct tagged-item]
  (into [] (conj curr-struct tagged-item)))

(defmethod insert-into-top :list
  [curr-struct tagged-item]
  (assoc curr-struct :value (conj (curr-struct :value) tagged-item)))

;; basically implements the built-in "read-string" function
;; need to figure out error checking with parentheses
;; not working from the terminal, but works in the REPL
(defn vectorify
  [tokens]
  (loop [i 0
         stack []]
   (if (>= i (count tokens))
     (do 
       (if (= (last tokens) ")")
        (first stack)
        (do 
          (println "ERROR")
          (System/exit -1)))))
    (do (let [t (nth tokens i)]
        (cond 
          (= t "(") (recur (inc i) (into [] (cons [] stack)))
          (or (= t ")") (= t "]"))
            (do (let [top-vec (first stack)
                      stack (rest stack)]                  
                  (if (empty? stack)
                    top-vec
                    (do (let [next-vec (first stack) stack (rest stack)]
                      (recur (inc i) (into [] (cons (insert-into-top next-vec top-vec) stack))))))))
          (= t "[") (recur (inc i) (into [] (cons {:type :list :value []} stack)))
          :else 
            (do (let [curr-struct (first stack)]
                (recur (inc i) (assoc stack 0 (insert-into-top curr-struct (tag t)))))))))))

(defn remove-blanks
  [string]
    (filter (fn[x] (not (empty? x))) (str/split string #"\s")))

(defn tokenize
  [string]
    (let [spaced-out (remove-blanks (str/replace string #"[\[\]()]" #(str " " % " ")))]
      spaced-out))

;; EVALUATING

(defn get-type
  [expr]
  (cond 
    (is-vector? expr) :call
    (= :literal (:type expr)) :literal
    (= :list (:type expr)) :list))

(defmulti evaluate get-type)

(defmethod evaluate :call
  [expr]
  (let [function (first expr)]
    (apply (function :value) (map #(evaluate %) (rest expr)))))

(defmethod evaluate :literal
  [expr]
  (:value expr))

(defmethod evaluate :list
  [expr]
  (map #(evaluate %) (:value expr)))

(defn -main
  [& args]
  (println "Little Lisp interpreter starting up")
  (println "Ready for use")
  (loop [line (read-line)]
    (if (= "quit" (str/lower-case line))
      (println "Quitting.")
      (do
        (println ">> " line)
        (println (evaluate (vectorify (tokenize line))))
        (recur (read-line))))))

