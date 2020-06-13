(ns on-lisp.chap14
  (:require
    [on-lisp.utils :refer [mac]]))
    
;;; 14 - Anaphoric Macros

;;; ---------------------------------------------------------------------------
;;; 14.1 Anaphoric Variants

(comment
 "In natural language, an anaphor is an expression which refers back in the)
 conversation. The most common anaphor in English is probably \"it\", as in
 \"Get the wrench and put it on the table\"")

(defmacro aif [test-form then-form & [else-form]]
  `(let [~'it ~test-form]
     (if ~'it ~then-form ~else-form)))

#_(aif 'a (list it 'b) 'b)

(defmacro awhen [test-form & body]
  `(aif ~test-form
        (do ~@body)))
#_(awhen 'a
    (list it 'b))

(defmacro awhile [expr & body]
  `(loop [~'it ~expr]
     (when ~'it
       ~@body
       (recur ~expr))))
#_(let [n (atom 5)]
    (awhile (> @n 0)
      (prn @n it)
      (swap! n dec)))

; `aand` is another implementation of `->` or `->>` or `as->`
(defmacro aand [& args]
  (cond (empty? args) true
        (empty? (rest args)) (first args)
        :else `(aif ~(first args) (aand ~@(rest args)))))
#_(aand)
#_(aand 1)
#_(aand 1 (inc it) (* it 3))

(comment
  "In the expansion of an acond clause, the result of the test expression
  will initially be kept in a gensymed variable, in order that the symbol
  `it` may be bound only within the remainder of the clause. When macros
  create bindings, they should always do so over the narrowest possible
  scope. Here, if we dispensed with the gensym and instead bound `it`
  immediately to the result of the test expression, then that binding would
  also have within its scope the following test expression.")

(defmacro acond [& clauses]
  (when (seq clauses)
    (let [cl1 (take 2 clauses)]
      `(let [tst# ~(first cl1)]
          (if tst#
            (let [~'it tst#]
              ~(last cl1))
            (acond ~@(drop 2 clauses)))))))
             

#_(mac
    (acond false it
           nil it
           (+ 1 1) it))

;;; Figure 14.2: More anaphoric variants

(comment 
  "`alambda` is unnecessary, as we can already do it using `recur` and
  anonymous functions can be named in clj.")

(defmacro alambda [params & body]
  `(letfn [(~'self ~params ~@body)]
     ~'self))

(defmacro alambda [params & body]
  `(fn ~'self ~params 
     ~@body))

(defn count-instances [obj colls]
  (map (alambda [coll]
          (if (seq coll)
            (+ (if (= (first coll) obj) 1 0)
               (self (rest coll)))
            0))
       colls))
#_(count-instances \a ["abc" "darpa" "dar" "aa"])

(defmacro block [tag form result]
  (if (coll? form)
    (let [[f s l :as form] form]
      `(if (and (= :block/return-from ~f)
                (= ~tag) ~s)
         ~l
         (do ~form ~result)))
    `(do ~form ~result)))
  
  
#_(mac
    (block :block/a
      (:block/return-from :block/a 1)
      3))
     