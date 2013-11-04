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

(defn tag
  [token]
  token)
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

