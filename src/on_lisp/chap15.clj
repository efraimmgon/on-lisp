(ns on-lisp.chap15
  (:require
    [on-lisp.utils :refer [mac atomp]]
    [on-lisp.chap05 :refer [lrec ttrav trec]]))
    
;;; 15 - Macros Returning Functions

;;; ---------------------------------------------------------------------------
;;; 15.1 Building Functions

(declare rbuild)

(defn build-compose [fns]
  (let [g (gensym)]
    `(fn [~g]
       ~(letfn [(rec [fns]
                  (if fns
                    `(~(rbuild (first fns))
                      ~(rec (next fns)))
                    g))]
           (rec fns)))))

(defn build-call [op fns]
  (let [g (gensym)]
    `(fn [~g]
       (~op ~@(map (fn [f]
                     `(~(rbuild f) ~g))
                   fns)))))

(defn rbuild [expr]
  (if (or (not (sequential? expr)) (= (first expr) 'fn))
    expr
    (if (= (first expr) 'comp)
      (build-compose (rest expr))
      (build-call (first expr) (rest expr)))))

(defmacro fn-by 
  "Builds compound functions from their descriptions. Its argument should
  be an expression of the form (operator . arguments). The operator can
  be the name of a function or macro - or `comp`, which is treated
  specially. The arguments can be names of functions or macros of one
  argument, or expressions that could be arguments to `fn-by`."
  [expr]
  (rbuild expr))

#_((fn-by (and integer? odd?)) 1)
#_((fn-by (comp list inc int)) 1.1)
#_((fn-by (comp (fn [x] (+ x 3)) int)) 1)
;; Using `and` as the operator yields the intersection of the operators
;; given as arguments:
#_(map (fn-by (and integer? odd?))
       '[c 3 p 0])
;; While `or` yields the union:
#_(map (fn-by (or integer? symbol?))
       '[c 3 p 0.2])
;; And if yields a function whose body is a conditional:
#_(map (fn-by (if odd? inc identity))
       (range 1 7))
;; However we can use other functions besides these three:
#_(map (fn-by (list dec identity inc))
       [1 2 3])
;; And the arguments in the `fn-by` expression may themselves be expressions:
#_(remove (fn-by (or (and integer? odd?)
                     (and coll? next)))
          '[1 [a b] c [d] 2 3.4 [e f g]])

;;; ---------------------------------------------------------------------------
;;; 15.2 Recursion on Cdrs


(defn our-every [f coll]
  (if-not (seq coll)
    true
    (and (f (first coll))
         (our-every f (rest coll)))))

;;; Figure 15.2: Macros for list recursion.

(defmacro alrec [rec & [base]]
  `(lrec (fn [~'it f#]
           (letfn [(~'rec [] (f#))]
             ~rec))
         ~base))

#_((lrec (fn [x f] (and (odd? x) (f))) true)
   [1 3 5])
#_((alrec (and (odd? it) (rec)) true)
   [1 3 5])
#_(def our-length
    (alrec (inc (rec)) 0))

(defmacro on-cdrs [rec base & [colls]]
  `((alrec ~rec (fn [] ~base))
    ~colls))

#_(on-cdrs (inc (rec)) 0 (range 5))
#_(on-cdrs (and (odd? it) (rec)) true
           [1 3 5])
#_
(defn our-length [coll]
  (on-cdrs (inc (rec)) 0 coll))
#_
(defn our-every [f coll]
  (on-cdrs (and (f it) (rec)) true coll)) 

;;; Figure 15.3: Common Lisp functions defined with on-cdrs.

#_
(defn our-copy-list [coll]
  (on-cdrs (cons it (rec)) nil coll))
#_
(defn our-remove-duplicates [coll]
  (letfn [(adjoin [coll x]
            (if (some #{x} coll)
              coll
              (conj coll x)))]
    (on-cdrs (adjoin (rec) it) nil coll)))
#_
(defn our-find-if [f coll]
  (on-cdrs (if (f it) it (rec)) nil coll))
#_
(defn our-some [f coll]
  (on-cdrs (or (f it) (rec)) nil coll))

;;; Figure 15.4: New utilities defined with on-cdrs.

(defn maxmin [args]
  (when (seq args)
    (on-cdrs (let [[mx mn] (rec)]
               [(max mx it) (min mn it)])
             [(first args) (first args)]
             (rest args))))
#_(maxmin [3 4 2 8 5 1 6 7])

;;; ---------------------------------------------------------------------------
;;; 15.3 Recursion on Subtrees

(defn our-copy-tree [tree]
  (if (seq tree)
    (cons (our-copy-tree (first tree))
          (when (next tree)
            (our-copy-tree (next tree))))
    tree))
#_(ttrav cons)

(defn rfind-if [f tree]
  (if (atomp tree)
    (and (f tree) tree)
    (or (rfind-if (first tree))
        (and (next tree)
             (rfind-if f (next tree))))))

#_ ;; `rfind-if` for `odd?` with `trec`'
(trec (fn [o l r] (or (l) (r)))
      (fn [tr] (and (odd? tr) tr)))

;;; Figure 15.5: Macros for recursion on tree

(defmacro atrec [rec & [base]]
  (let [base (or base 'it)]
    `(trec (fn [~'it lfn# rfn#]
             (letfn [(~'left [] (lfn#))
                     (~'right [] (rfn#))]
               ~rec))
           (fn [~'it] ~base))))
#_((atrec (cons (left) (right)))
   [1 [3 [5 6]]])
#_((atrec (or (left) (right)) 
          (and (odd? it) it))
   [0 2 [3 [5]]])