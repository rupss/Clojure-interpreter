(ns interpreter.core
  (:gen-class)
  (:require [clojure.string :as str]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn tokenize
  [string]
  (let [spaced-out (str/replace string #"[()]" #(str " " % " "))]
    (filter (fn[x] (not (empty? x))) (str/split spaced-out #"\\s"))))
