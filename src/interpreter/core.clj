;; On scoping - the env-stack grows from right to left i.e
;; the right-most env is the global environment and the left-most
;; env is the innermost scope.

(ns interpreter.core
  (:gen-class)
  (:require [clojure.string :as str]))

(def operator-map {"+" + "-" - "*" * "/" / "==" == ">" > "<" < "<=" <= ">=" >=})
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

(defn is-if?
  [token]
  (= token "if"))

(defn is-def?
  [token]
  (= token "def"))

(defn tag
  [token]
  (let [operator (operator-map token)
        str-literal (str-literal token)
        num-literal (num-literal token)
        bool (bool-map token)
        builtin (builtin-map token)
        is-if (is-if? token)
        is-def (is-def? token)]
  (cond
    (not (nil? operator)) {:type :operator :value operator}
    (not (nil? str-literal)) {:type :literal :value str-literal}
    (not (nil? num-literal)) {:type :literal :value num-literal}
    (not (nil? bool)) {:type :bool :value bool}
    (not (nil? builtin)) {:type :builtin :value builtin}
    (true? is-if) {:type :if}
    (true? is-def) {:type :def} 
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
(defn vectorify
  [tokens]
  (if (== 1 (count tokens))
    [(tag (first tokens))]
    (do
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
                     (recur (inc i) (assoc stack 0 (insert-into-top curr-struct (tag t)))))))))))))

(defn remove-blanks
  [string]
    (filter (fn[x] (not (empty? x))) (str/split string #"\s")))

(defn tokenize
  [string]
    (let [spaced-out (remove-blanks (str/replace string #"[\[\]()]" #(str " " % " ")))]
      spaced-out))

;; scoping helper methods

(defn get-global-scope
  "returns the global scope"
  [env-stack]
  (last (:scopes @env-stack)))

(defn modify-global-scope
  "Modifies the global scope in env-stack"
  [env-stack modification]
  (let [old-scopes (:scopes @env-stack)
        old-global-scope (get-global-scope env-stack)
        new-global-scope (merge old-global-scope modification)]
    (let [new-scopes (assoc (into [] old-scopes) (- (count old-scopes) 1) new-global-scope)]
      (swap! env-stack assoc :scopes new-scopes))))

(defn add-new-local-scope
  [new-scope env-stack]
  (swap! env-stack assoc :scopes (cons new-scope (:scopes @env-stack))))

(defn get-identifier-value
  "gets the most local value of the identifier if it exists, otherwise nil"
  [env-stack id]
  (let [scope (first (filter #(not (nil? (% id))) (:scopes @env-stack)))]
    (if (not (nil? scope))
      (id scope)
      nil)))

;; EVALUATING

(defn get-type
  [env-stack expr]
  ;; (println expr)
  ;; (print env)
  (cond
   (= :literal (:type (first expr))) :solitary-literal
   (= :literal (:type expr)) :literal
   (= :list (:type expr)) :list
   (= :identifier (:type expr)) :identifier
   (= :identifier (:type (first expr))) :solitary-identifier
   (= :if (:type (first expr))) :if
   (= :def (:type (first expr))) :def
   (= :let (:type (first expr))) :let
   :else :call))

(defmulti evaluate get-type)

(defmethod evaluate :let
  [env-stack expr]
  )

(defmethod evaluate :solitary-identifier
  [env-stack expr]
  (let [key (keyword (:value (first expr)))
        value (get-identifier-value env-stack key)]
    (if (nil? value)
      (println "ERROR - undefined var")
      value)))

(defmethod evaluate :identifier
  [env-stack expr]
  (println "IDENTIFIER")
  (println env-stack)
  (println expr)
  (let [key (keyword (:value expr))
        value (get-identifier-value env-stack key)]
    (if (nil? value)
      (println "ERROR - undefined var")
      value)))

(defmethod evaluate :def
  [env-stack [def-expr name value-expr]]
  (let [value (evaluate env-stack value-expr)]
    (modify-global-scope env-stack {(keyword (:value name)) value})
    nil))

(defmethod evaluate :if
  [env-stack [if-obj cond true-expr false-expr :as expr]]
  ;; (println "IF")
  ;; (println expr)
  (if (evaluate env-stack cond)
    (evaluate env-stack true-expr)
    (if false-expr
      (evaluate env-stack false-expr))))

(defmethod evaluate :call
  [env-stack expr]
  (println "CALL")
  (println expr)
  (let [function (first expr)]
    (apply (function :value) (map #(evaluate env-stack %) (rest expr)))))

(defmethod evaluate :literal
  [env-stack expr]
  (println "LITERAL")
  (println expr)
  (:value expr))

(defmethod evaluate :solitary-literal
  [env-stack expr]
  (:value (first expr)))

(defmethod evaluate :list
  [env-stack expr]
  (map #(evaluate env-stack %) (:value expr)))

;; (defn -main
;;   [& args]
;;   (clojure.main/repl :init (fn [] (println "Little Lisp interpreter starting up"))
;;                      :eval (fn [x] (run x))
;;                      :prompt (fn [] (print ">> "))
;;                      :read (fn [request-prompt request-exit]
;;                              (let [form (clojure.main/repl-read request-prompt request-exit)]
;;                                (if (= 'exit form) request-exit form)))))

(defn -main
  [& args]
  (println "Little Lisp interpreter starting up")
  (println "Ready for use")
  (let [env-stack (atom {:scopes [{}]})]
    (loop [line (read-line)]
      (if (= "quit" (str/lower-case line))
        (println "Quitting.")
        (do
          (println ">> " line)
          (let [vec (vectorify (tokenize line))]
            ;; (println "VEC")
            ;; (println vec)
            (println (evaluate env (vectorify (tokenize line)))))
          (recur (read-line)))))))


(def expr (vectorify (tokenize "(+ 1 2)")))
(def d (tokenize "(def x 2)"))
