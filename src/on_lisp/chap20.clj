(ns on-lisp.chap20
  (:require
    [on-lisp.utils :refer [mac cl-atom]]))

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
      
(comment
  "`=bind` is intended to be used in the same way as `multiple-value-bind`.
  It takes a list of parameters, an expression, and a body of code: the
  parameters are bound to the values returned by the expression, and the
  code body is evaluated with those bindinds. This macro should be used
  whenever additional expressions have to be evaluated after calling a
  function defined with `=defn`.")
(defmacro =bind [params expr & body]
  `(let [~'cont (fn ~params ~@body)] ~expr))

(comment
  "The syntax of `=values` is the same as that of the Common Lisp form
  `values`. It can return multiple values if there is an `=bind` with
  the same number of arguments waiting for them, but can't return multiple
  values to the toplevel.
  Note: Clojure has no support for multiple values, so this makes no sense.")
(defmacro =values [& retvals]
  `(~'cont ~@retvals))

(comment
  "`=apply` and `=funcall` are for use with functions defined with `=lambda`.
  Note that 'functions' defined with `=defn`, because they are actually
  macros, cannot be given as arguments to `apply` or `funcall`. The way 
  around is to package up the call inside another `=lambda`:"
  
  (=defn add1 [x] (=values (inc x)))

  (let [f (=lambda [n] (add1 n))]
    (=bind [y] (=funcall f 9)
      (println "9 + 1 =" y))))

(defmacro =funcall [f & args]
  `(~f ~'cont ~@args))

(defmacro =apply [f & args]
  `(apply ~f ~'cont ~@args))


(comment
  "Usage of `bind`:"
  
  (=defn message []
    (=values 'hello 'there))
  
  (=defn baz []
    (=bind [m n] (message)
      (=values (list m n))))
  
  (baz))
  
(comment
  "Figure 20.5: Restrictions on continuation-passing macros.
  
  1. The parameter list of a function defined with `=defn` must consist 
  solely of parameter names.
  
  2. Functions which make use of continuations, or call other functions which
  do, must be defined with `=lambda` or `=defn`.
  
  3. Such functions must terminate either by returning values with `=value`,
  or by calling another function which obeys this restriction.
  
  4. If an `=bind`, `=values`, `=apply`, or `=funcall` expression occurs in
  a segment of code, it must be tail call. Any code to be evaluated after an
  `=bind` should be put in its body. So if we want to have several `=binds`
  one after another, they must be nested:"

  (=defn foo [x]
    (=bind [y] (bar x)
      (print "Ho ")
      (=bind [z] (baz z)
        (print "Hum.")
        (=values x y z))))

  "Functions which neither save continuations, nor call other functions which 
  do, need not use these special macros. Built-in functions like `list`, for
  example, are exempt.")


;;; Figure 20.6: Tree traversal using continuation-passing macros.

(defn cl-null 
  "Returns true if `x` is empty. 
  Note: can't just use `empty?` because it expects a coll."
  [x]
  (and (seqable? x)
       (empty? x)))

(def saved (atom nil))

(=defn restart []
  (if (seq @saved)
    (let [f (peek @saved)]
      (swap! saved pop)
      (f))
    (=values :done)))

(defn dft [tree]
  (if (seqable? tree)
    (when (seq tree)
      (do (dft (first tree))
          (dft (rest tree))))
    (pr tree)))

(=defn dft-node [tree]
  (if (seqable? tree)
    (if (seq tree)
      (do (swap! saved conj #(dft-node (rest tree)))
          (dft-node (first tree)))
      (restart))
    (=values tree)))
       
(=defn dft2 [tree]
  (reset! saved nil)
  (=bind [node] (dft-node tree)
    (if (= node :done)
      (=values nil)
      (do (pr node)
          (restart)))))

(comment
  (def t1 '[a [b [d h]] [c e [f i] g]])
  (def t2 '[1 [2 [3 6 7] 4 5]])
  
  (dft2 t1)

  (=bind [node1] (dft-node t1)
    (if (= node1 :done)
      :done
      (=bind [node2] (dft-node t2)
        [node1 node2]))))
