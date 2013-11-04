(ns interpreter.core
  (:gen-class)
  (:require [clojure.string :as str]))

; (defn vectorify
;   [tokens]
;   (loop [i 0
;    stack []]
;    (println "BEGINNING")

;    (println "STACK")
;    (println stack)
;    (println "I = " i)

;    (if (and (>= i (count tokens))
;     (first stack)
;     (do
;       (let [t (nth tokens i)]
;         (println t)
;         (cond 
;           (= t "(") (recur (inc i) (into [] (cons [] stack)))
;             (= t ")")
;             (do 
;               (let [top-vec (first stack)]
;                 (println "TOP-VEC " top-vec)
;                 (let [stack (rest stack)]
;                   (println "stack = " stack)
;                   (if (empty? stack)
;                     top-vec
;                     (do
;                       (let [next-vec (first stack)]
;                         (println "stack before = " stack)
;                         (println "NEXT_VEC " next-vec)
;                         (let [stack (rest stack)]
;                           (println "stack after  = " stack)
;                           (recur (inc i) (into [] (cons (conj next-vec top-vec) stack))))))))))
;             :else 
;             (do 
;               (let [curr-vec (first stack)]
;                 (recur (inc i) (assoc stack 0 (into [] (conj curr-vec t))))))))))))

;; (defmulti name dispatch-fn & options)
;; (defmethod multifn dispatch-value & fn-tail)

(defn is-operator?
  [token]
  (or (= token "+") (= token "-") (= token "*") (= token "/")))

(defn is-str-literal?
  [token]
  (and (= (first token) (char 34)) (= (last token) (char 34))))

(defn is-num-literal?
  [token]
  (try
    (Integer/parseInt token)
    (catch Exception e
      false)))
  
(defn is-boolean?
  [token]
  (or (= token "false") (= token "true")))

(defn get-token-type
  [token]
  (cond
    (is-operator? token) :operator
    (is-str-literal? token) :str-literal
    (is-num-literal? token) :num-literal
    (is-boolean? token) :bool))

(defmulti tag get-token-type)

(defmethod tag :operator
  [token]
  {:type :operator :value token})

(defmethod tag :str-literal
  [token]
  {:type :literal :value token})

(defmethod tag :num-literal
  [token]
  {:type :literal :value (is-num-literal? token)})

(defmethod tag :default
  [token]
  {:type :identifier :value token})

(defmethod tag :bool
  [token]
  {:type :bool :value token})

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
            (= t ")")
            (do (let [top-vec (first stack)
                      stack (rest stack)]
                  (if (empty? stack)
                    top-vec
                    (do (let [next-vec (first stack) stack (rest stack)]
                          (recur (inc i) (into [] (cons (conj next-vec top-vec) stack))))))))
            :else 
            (do (let [curr-vec (first stack)]
                (recur (inc i) (assoc stack 0 (into [] (conj curr-vec (tag t))))))))))))

(defn remove-blanks
  [string]
    (filter (fn[x] (not (empty? x))) (str/split string #"\\s")))

(defn tokenize
  [string]
  (let [spaced-out (remove-blanks (str/replace string #"[()]" #(str " " % " ")))]
    spaced-out))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!")
  (println (vectorify (tokenize "(+ 1 2)"))))

