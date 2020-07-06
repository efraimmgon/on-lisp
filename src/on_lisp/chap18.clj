(ns on-lisp.chap18
  (:require
    [on-lisp.utils :refer [mac cl-atom acond]]
    clojure.set))

;;; 18 - Destructuring

;;; ---------------------------------------------------------------------------
;;; 18.2 Other Structures

;;; 18.1 General sequence destructuring operator

(defn destruc
  "Traverses the pattern and associates each variable with the location of 
  the corresponding object at runtime.
  The optional third argument is the predicate used to distinguish pattern
  structure from pattern content."
  [pat seqn & [atom? n]]
  (let [atom? (or atom? cl-atom)
        n (or n 0)]
    (when (seq pat)
      (let [more (cond (atom? pat) pat
                       (= (first pat) '&) (first (rest pat))
                       :else nil)]
        (if more
          `[~more (drop ~n ~seqn)]
          (let [p (first pat)
                rec (destruc (rest pat) seqn atom? (inc n))]
            (if (atom? p)
              (vec
                (concat `(~p (nth ~seqn ~n))
                        rec))
              (let [var (gensym)]
                (vec
                  (concat (concat `(~var (nth ~seqn ~n))
                                  (destruc p var atom?))
                          rec))))))))))
      
   

#_(destruc '[a b c] 'seq cl-atom)
#_(destruc '[a [b c] & d] 'seq)

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
  
(defn binding 
  "Returns the bindings (if any) for `x`."
  [x binds]
  (letfn [(recbind [x binds]
            (when-let [it (get binds x)]
              (or (recbind it binds)
                  it)))]
    (let [b (recbind x binds)]
      b)))

(defn match
  "Compares `x` and `y` element by element and builds up assingments of values
  to variables (bindings), in the paramenter `binds`. If the match is 
  successful, returns the bindings generated, otherwise returns `nil`." 
  ([x y]
   (match x y {}))
  ([x y binds]
   (acond
     ;; If they're equal or it's the wildcard symbol, we just return the
     ;; bindings.
     (or (= x y) (= x '_) (= y '_)) binds
     
     ;; If there's a binding for x or y we run match with it.
     (binding x binds) (match it y binds)
     
     (binding y binds) (match x it binds)
     
     ;; If either x or y is a variable, we assoc it with their match.
     (varsym? x) (assoc binds x y)
     
     (varsym? y) (assoc binds y x)
     
     ;; If x and y are a coll we run match with the 1st elt, and with the next,
     ;; if it's successful.
     (and (seqable? x) (seqable? y) 
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

(def var? varsym?)
(assert (not (var? 'x)))
(assert (var? '?x))

(defn vars-in 
  "Returns all the pattern variables in an expression."
  ([expr] 
   (vars-in expr cl-atom))
  ([expr atom?]
   (if (atom? expr)
     (when (var? expr)
        #{expr})
     (clojure.set/union 
       (vars-in (first expr) atom?)
       (vars-in (next expr) atom?)))))

(defmacro if-match
  "The first two arguments are a pattern and a sequence, and it establishes
  bindings by comparing the pattern with the sequence. However, instead
  of a body it has two more arguments: a `then` clause to be evaluated, with
  new bindings, if the match succeeds; and an `else` clause to be evaluated
  if the match fails." 
  ([pat seqn then]
   (if-match pat seqn then nil))
  ([pat seqn then else]
   `(if-let [~'it (match '~pat ~seqn)]
      (let [~@(mapcat (fn [v]
                        `(~v (binding '~v ~'it)))
                      (vars-in then cl-atom))]
        ~then)
      ~else)))

#_
(mac
  (if-match [?x ?y ?x ?y] '[hi ho hi ho]
    [?x ?y]
    nil))

(defn abab [seqn]
  (if-match [?x ?y ?x ?y] seqn
    [?x ?y]
    nil))
#_
(abab '[hi ho hi ho])
#_
(abab "abab")


;;; Figure 18.7 & 18.8: Fast matching operator.

;; buggy
; (defn length-test [pat coll]
;   (let [fin (first (last coll))]
;     (if (or (coll? fin) (= (name fin) "nth"))
;       `(= (count ~pat) ~(count coll))
;       `(> (count ~pat) ~(- (count (partition 2 coll)) 2)))))
    

; (defn gensym? [s]
;   (and (symbol? s)
;        (= "G__" (subs (name s) 0 3))))

; (defn match1
;   "If the pattern argument is a gensym, then it is one of the invisible
;   variables created by `destruc` to hold sublists, and all we need to do
;   at runtime is test that it has the right length.
;   If the pattern element is a wildcard (_) no code need be generated.
;   If the pattern element is a variable, we generate code to match it against,
;   or set it to, the corresponding part of the sequence given at runtime.
;   Otherwise, the pattern element is taken to be a literal value, and we 
;   generate code to compare it with the corresponding part of the sequence."
;   [refs then else]
;   (let [[pat expr] (take 2 refs)
;         more (drop 2 refs)]
;     (cond
;       (gensym? pat)
;       `(let [~pat ~expr]
;          (if (and (coll? ~pat)
;                   ~(length-test pat more))
;            ~then
;            ~else))
      
;       (= pat '_) then
      
;       (var? pat)
;       (let [ge (gensym)]
;         `(let [~ge ~expr]
;            (if (or (gensym? ~pat) (= ~pat ~ge))
;              (let [~pat ~ge] ~then)
;              ~else)))
      
;       :else `(if (= ~pat ~expr) ~then ~else))))

; (comment
;   "The distinction between pattern content and pattern structure will be
;   defined by the function `simple?`. If we want to be able to use quoted
;   literals in patterns, the destructuring code (and `vars-in`) have to be
;   told not to go inside seqs whose first elements is `quote`. With the new
;   matching operator, we will be able to use lists as pattern elements,
;   simply by quoting them.")

(defn simple? [x]
  (or (cl-atom x) 
      (= (first x) 'quote)))

; (defn gen-match 
;   "Recursively generates matching code for nested patterns, and thence to
;   `match1`, which generates match code for each leaf of the pattern tree."
;   [refs then else]
;   (if-not (seq refs)
;     then
;     (let [then (gen-match (drop 2 refs) then else)]
;       (if (simple? (first refs))
;         (match1 refs then else)
;         (gen-match (take 2 refs) then else)))))

; (defmacro pat-match [pat seqn then else]
;   (if (simple? pat)
;     (match1 `(~pat ~seqn) then else)
;     (let [gseq (gensym)
;           gelse (gensym)]
;       `(letfn [(~gelse [] ~else)]
;          ~(gen-match (->> (destruc pat gseq simple?)
;                           (concat (list gseq seqn)))
;                      then
;                      `(~gelse))))))

; (defmacro if-match 
;   ([pat seqn then]
;    (if-match pat seqn then nil))
;   ([pat seqn then else]
;    `(let [~@(mapcat (fn [v] 
;                       `(~v '~(gensym)))
;                     (vars-in pat simple?))]
;       (pat-match ~pat ~seqn ~then ~else))))
      
                          

; (comment
;   "By restricting variables to the first argument of `if-match`, we make it
;   possible to tell at compile-time which varibles will be involved in the
;   match. Then instead of creating lists of variable bindings, we could 
;   keep the values of variables in the variables themselves.")

; ; pat-match
; (comment 
;   "Takes the same arguments as `if-match`; the only difference is
;   that it establishes no new bindings for pattern variables. In some 
;   situations this is an advantage.")

; #_
; (if-match [?x 'a] seq
;   (prn ?x)
;   nil)

; #_(destruc '[?x 'a] 'g simple?)
; #_(destruc '[a [b c] & d] 'seq)

; (comment
;   "In the new `if-match`, the pattern elements are now evaluated instead of
;   being implicitly quoted. This means that Lisp variables can be used in
;   patterns, as well as quoted expressions:")
; #_
; (let [n 3]
;   (if-match [?x n 'n '[a b]] '[1 3 n [a b]]
;     ?x))

; (comment
;   "The pattern can now contain rest (&) arguments.")
; #_
; (if-match [?x [1 & ?y] ?x] '[[a b] [1 2 3] [a b]]
;   [?x ?y])