# Clojure-interpreter

Writing a mini Clojure interpreter, implemented in Clojure. 

## Prerequisites

You will need Leiningen 2.0 or above installed.

## Usage

   lein run

## Status

Right now, this interpreter handles: 

- integer and string literals
- arithmetic expressions using +, -, /, *
- logical comparisons using ==, >, <, >=, <=, not
- if statements
  (if <CONDITION> <EXPR> <ELSE-EXPR>)
- defining global Vars using def
- let statements
- lists and the conj, cons, first, last, rest functions

## License

Copyright © 2013 Rupa Shankar
