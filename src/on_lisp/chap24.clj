(ns on-lisp.chap24
  (:require
    [on-lisp.utils :refer [mac cl-atom]]
    [on-lisp.chap18 :refer [vars-in binding]]
    [on-lisp.chap20 :refer [=bind =values =defn =bind cont]]
    [on-lisp.chap22 :refer [choose fail paths]]))

;;; 24 Prolog

;;; ---------------------------------------------------------------------------
;;; 24.2 An Interpreter

(defn varsym? [x]
  (and (symbol? x)
       (= \? (first (name x)))))
#_(varsym? '?x)

(comment
  "The function `fullbind` points to another difference between `with-answer`
  and `with-inference`. Tracing back through a series of rules can build up
  binding lists in which the binding of a variable is a list of other
  variables. To make use of the results of a query we now need a recursive
  function for retrieving bindings.")

(defn fullbind [x b]
  (newline)
  (prn 'x '=> x)
  (prn 'b '=> b)
  (cond
    (varsym? x) (if-let [it (binding x b)]
                  (fullbind it b)
                  (gensym))
    (cl-atom x) x
    :else (cons (fullbind (first x) b)
                (fullbind (next x) b))))
(let [b '{?x [?y ?z] ?y foo ?z nil}]
  (prn (binding '?x b))
  (prn (fullbind '?x b)))


(defn rep_ [x]
  (if (cl-atom x)
    (if (= x '_) 
      (gensym "?") 
      x)
    (cons (rep_ (first x))
          (rep_ (rest x)))))

(defmacro with-inference [query & body]
  `(do
     (reset! paths nil)
     (=bind [binds] (prove-query '~(rep_ query) nil)
       (let [~@(mapcat (fn [v]
                         `(~v (fullbind '~v binds)))
                       (vars-in query cl-atom))]
         ~@body
         (fail)))))

#_
(with-inference (painter ?x)
  (print ?x))