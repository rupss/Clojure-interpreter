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
- logical comparisons using ==, >, <, >=, <=, <code>not</code>
- if statements <code>(if CONDITION EXPR ELSE-EXPR)</code>
- defining global Vars using <code>def</code>
- <code>let</code> statements
- vectors and the <code>conj, cons, first, last, rest</code> functions

At present, its error handling is minimal. This will change soon.

## License

Copyright © 2013 Rupa Shankar
