(ns on-lisp.chap18
  (:require
    [on-lisp.utils :refer [mac cl-atom acond acond2]]))

;;; 18 - Destructuring

;;; ---------------------------------------------------------------------------
;;; 18.2 Other Structures

(defmacro with-array [pat ar & body]
  (let [gar (gensym)]
    `(let [~gar ~ar]
       (let [~@(mapcat (fn [p]
                         `(~(first p) (get-in ~gar ~(vec (rest p)))))
                       pat)]
         ~@body))))
#_
(def arr [[0 1 2]
          [10 11 12]
          [20 21 22]])
#_
(mac
  (with-array
    [[a 0 0]
     [d 1 1]
     [i 2 2]] arr
    (list a d i)))

;;; ---------------------------------------------------------------------------
;;; 18.4 Matching

(comment
  "To be able to write such an operator we have to invent some way of
  distinguising variable. We can't just say that all symbols are variables,
  because we will want symbols to occur as arguments within patterns. Here
  we will say that a pattern variable is a symbol beginning with a question
  mark. If it becomes inconvenient, this convention could be changed symply
  by redefining the predicate `var?`.")

(defn var? 
  "Is the symbol a pattern variable?"
  [x]
  (and (symbol? x)
       (-> (name x)
           first
           (= \?))))
(assert (not (var? 'x)))
(assert (var? '?x))

(comment
  "As `match` compares its arguments element by element, it builds up
  assignments of values to variables, called `bindings`, in the parameter
  `binds`. If the match is successful, `match` returns the bindings
  generated, otherwise it returns nil.")
(comment
  "We don't need to return a second value indicating whether the matching
  was successful. If it was successful, even though there were no matchings,
  it will return an empty map. If it was not it will return nil")

(defn varsym? 
  "Is the symbol a pattern variable?"
  [x]
  (and (symbol? x)
       (= \?
          (first (name x)))))
  
(defn binding [x binds]
  (letfn [(recbind [x binds]
            (when (seq binds)
              (when-let [it (get binds x)]
                (or (recbind it binds)
                    it))))]
    (let [b (recbind x binds)]
      b)))

(defn match 
  ([x y]
   (match x y {}))
  ([x y binds]
   (acond
     (or (= x y) (= x '_) (= y '_)) binds
     
     (binding x binds) (match it y binds)
     
     (binding y binds) (match x it binds)
     
     (varsym? x) (assoc binds x y)
     
     (varsym? y) (assoc binds y x)
     
     (and (coll? x) (coll? y) 
          (match (first x) (first y) binds))
     (match (next x) (next y) it)
     
     :else nil)))

#_
(match '[p a b c a] '[p ?x ?y c ?x])
#_
(match '[p ?x b ?y a] '[p ?y b c a])
#_
(match '(a b c) '(a a a))

(comment
  "Like Prolog, `match` treats `_` (underscore) as a wild-card. It matches
  everything, and has no effect on the bindings.")
#_
(match '[a ?x b] '[_ 1 _]) ; => ((?x 1))
#_
(match '[p ?x] '[p ?x])

;;; Figure 18.6: Slow matching operator.

(defn union [x y])

(defn vars-in 
  ([expr] 
   (vars-in expr cl-atom))
  ([[x & more :as expr] atom?]
   (if (atom? expr)
     (when (var? expr)
       (list expr))
     (union (vars-in x atom?)
            (vars-in more atom?)))))
