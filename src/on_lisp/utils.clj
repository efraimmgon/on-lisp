(ns on-lisp.utils)

; (consp nil) => false
(defn consp [x]
  (and (seqable? x)
       (seq x)))

; (atomp nil) => true
(defn atomp [x]
  (not (consp x)))

(defn member
  ([x coll]
   (member x nil coll))
  ([x {:keys [test]
       :or {test =}} 
    coll]
   (loop [remain coll]
     (when (seq remain)
       (if (test (first remain) x)
         remain
         (recur (rest remain)))))))

(defn mkcoll [x]
  (if (coll? x) x [x]))

(defmacro mac [expr]
  `(clojure.pprint/pprint (macroexpand-1 '~expr)))

(defmacro when-bind* 
  "Takes a list of pairs of the form `symbol` `expression`. If any
  expression returns nil, the whole when-bind* expression returns nil.
  Otherwise the whole body will be evaluated with each symbol bound as if 
  by let."
  [binds & body]
  (if (empty? binds)
    `(do ~@body)
    `(let ~(vec (take 2 binds))
       (when ~(first binds)
         (when-bind* ~(vec (drop 2 binds)) ~@body)))))

(defmacro with-gensyms 
  "Binds a list of variables to gensyms."
  [syms & body]
  `(let [~@(interleave syms (repeat '(gensym)))]
     ~@body)) 

;;; CONDLET

(defn condlet-binds [vars cl]
  ;; Return the gensym paired with its form
  (mapcat (fn [[sym form :as bindform]]
            (when (seq bindform)
              [(some #(and (= sym (first %))
                           (last %))
                     vars)
               form]))
          ;; Return the bindings
          (->> (last cl)
               (partition 2))))
                   
(defn condlet-clause [vars cl bodfn]
  ;; Set the gensyns to nil...
  `(let [~@(interleave (map last vars)
                       (repeat nil))]
     ;; ... before using the user bindings. The previous bindings
     ;; will be the default (nil) in case there are no user bindings.
     (let [~@(condlet-binds vars cl)]
       (~bodfn ~@(map last vars)))))
           
(defmacro condlet
  "Takes a list of binding clauses, followed by a body of code. Each
  of the binding clauses is guarded by a test expression; the body
  of code will be evaluated with the bindings specified by the first 
  binding clause whose test expression returns true. Variables which
  occur in some clauses and not others will be bound to nil if the
  successful clause does not specify bindings for them."
  [clauses & body]
  (let [bodfn (gensym)
        vars (->> clauses
                  (partition 2)
                  (mapcat last)
                  (partition 2)
                  (map first)
                  distinct
                  ;; return the distinct vars with a gensym
                  (map #(vector % (gensym))))]
    `(letfn [(~bodfn ~(mapv first vars)
                ~@body)]
       (cond ~@(interleave (map first (partition 2 clauses))
                           (map #(condlet-clause vars % bodfn)
                                (partition 2 clauses)))))))

(defn pr-ret [& args]
  (apply pr args)
  args)

#_
(mac
  (condlet
    [(= 1 2) [x (pr-ret 'a) 
              y (pr-ret 'b)]
     (= 1 1) [y (pr-ret 'c) 
              x (pr-ret 'd)]
     :else   [x (pr-ret 'e) 
              z (pr-ret 'f)]]
    (list x y z)))

(defmacro if3 
  "Decides which form to evaluate based on three categories of truth:
  true, false, and uncertain, represented as `?`"
  [test t-case f-case ?-case]
  `(case ~test
     (false nil) ~f-case
     ? ~?-case
     ~t-case))

#_(while (not sick)
    (if3 (cake-permitted)
         (eat-cake)
         (throw 'tantrum nil)
         (plead-insistently)))

(defmacro nif
  "Numeric if. Takes a numeric expression as its first argument, and 
  depending on its sign evaluates one of the remaining three arguments."
  [expr pos zero neg]
  `(let [g# ~expr]
     (cond (pos? g#)  ~pos
           (zero? g#) ~zero
           :else      ~neg)))

#_
(map #(nif % 'p 'z 'n) [0 1 -1])

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
