(ns on-lisp.chap20
  (:require
    [on-lisp.utils :refer [mac]]))

;;; 20 - Continuations

;;; ---------------------------------------------------------------------------
;;; 20.1 Scheme Continuations

(comment
  "A continuation is a function representing the future of a computation.")

(comment
  "Continuations can be understood as a generalization of closures. A closure
  is a function plus pointers to the lexical variables visible at the time
  it was created. A continuation is a function plus a pointer to the whole
  stack pending at the time it was created. When a continuation is evaluated,
  it returns a value using its own copy of the stack, ignoring the current one.")

;;; ---------------------------------------------------------------------------
;;; 20.2 Continuation-Passing Macros

(comment
  "Scheme continuations gave us two things:
  1. The bindings of all variables at the time the continuation was made.
  2. The state of the computation - what was going to happen from then on.")

(comment
  "Instead of defining just a function, `=defn` defines a function and a
  macro which expands into a call to it. (The macro must be defined first,
  in case the function calls itself.)")

;;; Figure 20.4: Continuation-passing macros.


(def cont identity)

(defmacro =lambda [params & body]
  `(fn [~'cont ~@params] ~@body))

(defmacro =defn [sname params & body]
  (let [f (symbol (str "=" (name sname)))]
    `(do
       (declare ~f)
       (defmacro ~sname ~params
         `(~'~f ~'~'cont ~~@params))
       (defn ~f [~'cont ~@params] ~@body))))
      
(defmacro =bind [params expr & body]
  `(let [~'cont (fn ~params ~@body)] ~expr))

#_(mac (=defn add1 [x] (inc x)))