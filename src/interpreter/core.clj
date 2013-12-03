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

(declare evaluate)

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

(defn is-let?
  [token]
  (= token "let"))

(defn is-identifier?
  [token]
  (= :identifier (:type token)))

(defn tag
  [token]
  (let [operator (operator-map token)
        str-literal (str-literal token)
        num-literal (num-literal token)
        bool (bool-map token)
        builtin (builtin-map token)
        is-if (is-if? token)
        is-def (is-def? token)
        is-let (is-let? token)]
  (cond
    (not (nil? operator)) {:type :operator :value operator}
    (not (nil? str-literal)) {:type :literal :value str-literal}
    (not (nil? num-literal)) {:type :literal :value num-literal}
    (not (nil? bool)) {:type :bool :value bool}
    (not (nil? builtin)) {:type :builtin :value builtin}
    (true? is-if) {:type :if}
    (true? is-def) {:type :def}
    (true? is-let) {:type :let}
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
(defn vectorize
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

(defn remove-most-recent-scope
  [env-stack]
  (swap! env-stack assoc :scopes (rest (:scopes @env-stack))))

(defn get-identifier-value
  "gets the most local value of the identifier if it exists, otherwise nil"
  [env-stack id]
  (let [scope (first (filter #(not (nil? (% id))) (:scopes @env-stack)))]
    (if (not (nil? scope))
      (id scope)
      nil)))

(defn get-identifier-keyword
  "Given an identifier, returns the keyword of its value"
  [id]
  (keyword (:value id)))

(defn add-let-var
  [curr-map [id value-expr] env-stack]
  (merge curr-map {(get-identifier-keyword id) (evaluate env-stack value-expr)}))

(defn get-let-vars
  [[let-expr let-list & rest] env-stack]
  (let [var-list (:value let-list)]
    (reduce (fn [curr-map var-item] (add-let-var curr-map var-item env-stack)) {} (partition 2 var-list))))

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
  [env-stack [let-expr let-list & exprs :as expr]]
  (add-new-local-scope (get-let-vars expr env-stack) env-stack)
  (doseq [expr (subvec (into [] exprs) 0 (- (count exprs) 1))]
    (evaluate env-stack expr))
  (let [ret-val (evaluate env-stack (last exprs))]
    (remove-most-recent-scope env-stack)
    ret-val))

(defmethod evaluate :solitary-identifier
  [env-stack expr]
  (let [key (get-identifier-keyword (first expr))
        value (get-identifier-value env-stack key)]
    (if (nil? value)
      (println "ERROR - undefined symbol")
      value)))

(defmethod evaluate :identifier
  [env-stack expr]
  ;; (println "IDENTIFIER")
  ;; (println env-stack)
  ;; (println expr)
  (let [key (get-identifier-keyword expr)
        value (get-identifier-value env-stack key)]
    ;; (println value)
    (if (nil? value)
      (println "ERROR - undefined symbol")
      value)))

(defmethod evaluate :def
  [env-stack [def-expr name value-expr]]
  (let [value (evaluate env-stack value-expr)]
    (modify-global-scope env-stack {(get-identifier-keyword name) value})
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
  ;; (println "ENV_STACK")
  ;; (println env-stack)
  ;; (println "CALL")
  ;; (println expr)
  (let [function (first expr)]
    (apply (function :value) (map #(evaluate env-stack %) (rest expr)))))

(defmethod evaluate :literal
  [env-stack expr]
  ;; (println "LITERAL")
  ;; (println expr)
  (:value expr))

(defmethod evaluate :solitary-literal
  [env-stack expr]
  (:value (first expr)))

(defmethod evaluate :list
  [env-stack expr]
  (map #(evaluate env-stack %) (:value expr)))

;;;;;;

(defn sublist
  [my-list start & end]
  (if (empty? end)
    (subvec (into [] my-list) start)
    (subvec (into [] my-list) start (first end))))

(def matching-parens {\) \(
                     \] \[})

(defn is-balanced?
  [input]
  (if (empty? input)
    true)
  (do
    (loop [i 0
           stack []]
      (if (== i (count input))
        (empty? stack)
        (let [curr (nth input i)]
          (if (or (= curr \() (= curr \[))
            (recur (inc i) (cons curr stack))
            (if (or (= curr \)) (= curr \]))
              (if (= (matching-parens curr) (first stack))
                (recur (inc i) (rest stack))
                false))))))))

(defn correct-parentheses?
  [input]
  (let [parens-only (str/replace input #"[^()\[\]]" "")]
    (and (= (first parens-only) \() (= (last parens-only) \)) (is-balanced? (subs parens-only 1 (- (count parens-only) 1))))))

(defn -main
  [& args]
  (println "Little Clojure interpreter starting up")
  (println "Ready for use. Type \"quit\" to exit.")
  (let [env-stack (atom {:scopes [{}]})
        all-input (atom {:line nil})]
    (loop [line (read-line)]
      (if (= "quit" (str/lower-case line))
        (println "Quitting.")
        (do
          (swap! all-input assoc :line (str (@all-input :line) line))
          (println ">> " line)
          (if (correct-parentheses? (@all-input :line))
            (do
              (let [input (@all-input :line)]
                (println (evaluate env-stack (vectorize (tokenize input))))
                (swap! all-input assoc :line nil))))
          (recur (read-line)))))))


(def expr (vectorize (tokenize "(+ 1 2)")))
(def d (tokenize "(def x 2)"))
(def l (tokenize "(let [x 2
y 3] x)"))
(def ll (tokenize "(let [x (+ 1 1)] ((* 3 x))"))
(def var-list (:value (nth (vectorize l) 1)))
