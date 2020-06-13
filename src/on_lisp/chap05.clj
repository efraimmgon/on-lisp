(ns on-lisp.chap05
  (:require
    [on-lisp.utils :refer [mkcoll atomp]]))

;;;; Returning Functions

;;; ---------------------------------------------------------------------------
;;; 5.3 Memoizing

(defn memoize* [f]
  (let [cache (atom {})]
    (fn [& args]
      (if-let [val (get @cache args)]
        val
        (let [ret (apply f args)]
          (swap! cache assoc args ret)
          ret)))))

#_(def slowid (memoize* (fn [x] (Thread/sleep 5000) x)))
#_(time (slowid 1))
#_(time (slowid 1))

;;; ---------------------------------------------------------------------------
;;; 5.4 Composing Functions

(defn compose
  "Takes any number of functions and returns their composition. All
  the functions must have one argument, except the last, and whatever
  arguments it takes, so will the function returned by `compose`."
  [& fns]
  (if (seq fns)
    (let [fns (reverse fns)]
      (fn [& args]
        (reduce (fn [acc f]
                  (f acc))
                (apply (first fns) args)
                (rest fns))))
    identity))
#_((compose list inc) 1)

(defn complement* [pred]
  (compose not pred))

(defn fif
  "`if` for functions. If `test` returns true with `x`, calls `then` 
  on the `x`, otherwise calls `else`."
  ([test then] (fif test then nil))
  ([test then else]
   (fn [x]
     (if (test x)
       (then x)
       (when else
         (else x))))))
#_(map (fif odd? dec identity) (range 5))

(defn fint
  "Returns the intersection of a set of predicates."
  [f & fns]
  (if (empty? fns)
    f
    (let [chain (apply fint fns)]
      (fn [x]
        (and (f x) (chain x))))))
#_(filter (fint signed sealed delivered)
          docs)
; same as:
#_(filter (fn [x]
            (and (signed x) (sealed x) (delivered x)))
          docs)

(defn fun 
  "Returns the union of a set of predicates."
  [f & fns]
  (if (empty? fns)
    f
    (let [chain (apply fun fns)]
      (fn [x]
        (or (f x) (chain x))))))
#_(filter (fun signed sealed delivered)
          docs)
; same as:
#_(filter (fn [x]
            (or (signed x) (sealed x) (delivered x)))
          docs)

;;; ---------------------------------------------------------------------------
;;; 5.5 Recursion on Cdrs

(defn lrec
  "List recurser. The first arg must be a function of two args: the
  current first element of the coll and a function which can be called
  to continue the recursion."
  ([rec] (lrec rec nil))
  ([rec base]
   (letfn [(self [coll]
             (if (empty? coll)
               (if (fn? base)
                 (base)
                 base)
               (rec (first coll)
                    (fn []
                      (self (rest coll))))))]
     self)))
;; count:
#_((lrec (fn [x f] (inc (f))) 0) 
   (range 5))
;; every odd?:
#_((lrec (fn [x f] (and (odd? x) (f))) true)
   (range 1 5 2))

;;; ---------------------------------------------------------------------------
;;; 5.6 Recursion on Subtrees

(defn our-copy-tree [tr]
  (if (seqable? tr)
    (when (seq tr)
      (cons (our-copy-tree (first tr))
            (when (next tr)
              (our-copy-tree (next tr)))))
    tr))
#_(our-copy-tree [1 2 [3 4 [5]]])
    
(defn rfind-if [f tree]
  (if (seqable? tree)
    (when (seq tree)
      (or (rfind-if f (first tree))
          (when (next tree)
            (rfind-if f (next tree)))))
    (and (f tree) tree)))
#_(rfind-if (fint number? odd?) [2 [3 4] 5])

(defn ttrav
  "Tree traverser. It takes two args, one for the left subtree and one
  for the right. If the base argument is a function it will be called
  on the current leaf. In flat list recursion, the base case is always
  nil, but in tree recursion the base case could be an interesting value
  and we might want to use it."
  ([rec] 
   (ttrav rec identity))
  ([rec base]
   (letfn [(self [tree]
             (if (seqable? tree)
               (when (seq tree)
                 (rec (self (first tree))
                      (when (next tree)
                        (self (next tree)))))
               (if (fn? base)
                 (base tree)
                 base)))]
     self)))
;; our-copy-tree:
#_((ttrav cons) [1 2 [3 4 [5]]])
;; count-leaves
#_((ttrav (fn [l r] (inc (or r 1))) 1)
   [1 2 [3 4 [5]]])
;; flatten
#_((ttrav concat mkcoll)
   [1 2 [3 4 [5]]])

(defn trec
  "Tree recurser. 1st arg should be a function of three arguments: 
  the current object and the two recursers. The latter two will be closures
  representing the recursions down the left and right subtrees.
  Second arg is the base case, which may be a function."
  ([rec]
   (trec rec identity))
  ([rec base]
   (letfn [(self [tree]
             (if (seqable? tree)
               (when (seq tree)
                 (rec tree
                      #(self (first tree))
                      #(when-let [nxt (next tree)]
                         (self nxt))))
               (if (fn? base)
                 (base tree)
                 base)))]
     self)))
;; flatten:
#_((trec (fn [o l r] (into (l) (r)))
         mkcoll)
   [1 2 [3 4 [5]]])
;; rfind-if for odd?
#_((trec (fn [o l r] (or (l) (r)))
         (fn [tree] (and (odd? tree) tree)))
   [0 2 [3 4 [5]]])