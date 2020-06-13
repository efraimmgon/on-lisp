(ns on-lisp.chap11
  (:require
    [on-lisp.utils :refer [mac]]))

;;;; 11 Classic Macros

;;; ---------------------------------------------------------------------------
;;; 11.1 Creating Context

(defmacro our-let [binds & body]
  `((fn ~(mapv first
               (partition 2 binds))
      ~@body)
    ~@(map last 
           (partition 2 binds))))

#_
(mac
  (our-let 
    [x 1 y 2] 
    (+ x y)))


;;; Figure 11.2 - Macros which bind variables.

(defmacro when-bind [[var expr] & body]
  `(let [~var ~expr]
     (when ~var
       ~@body)))

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
#_
(mac
  (when-bind* [x (some #(and (coll? %) %) '[a [1 2] b])
               y (some #(and (odd? %) %) x)]
    (+ y 10)))

(defmacro with-gensyms 
  "Binds a list of variables to gensyms."
  [syms & body]
  `(let [~@(interleave syms (repeat '(gensym)))]
     ~@body)) 
#_
(mac
  (with-gensyms [x y w z] 
    (list x y w z)))


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

#_
(def clauses
  (quote
    [(= 1 2) [x (pr-ret 'a) 
              y (pr-ret 'b)]
     (= 1 1) [y (pr-ret 'c) 
              x (pr-ret 'd)]
     :else   [x (pr-ret 'e) 
              z (pr-ret 'f)]]))
#_
(def cl (take 2 clauses))

#_
(def vars
  (->> clauses
       (partition 2)
       (mapcat last)
       (partition 2)
       (map first)
       distinct
       (map #(vector % (gensym)))))

;;; ---------------------------------------------------------------------------
;;; 11.2 The with- Macro

(defmacro with-db [db & body]
  `(try
     (binding [*db* ~db]
       (lock *db*)
       ~@body)
     (catch Exception e)
     (finally
       (release *db*))))


;;; ---------------------------------------------------------------------------
;;; 11.3 Conditional Evaluation

;;; Figure 11.5 - Macros for conditional evaluation

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

;;; Figure 11.6 - Macros for conditional evaluation.

; I don't think in is necessary in clojure, because we have sets.
(defmacro in
  "Efficiently test for membership."
  [obj & choices]
  (let [objsym (gensym)]
    `(let [~objsym ~obj]
       (or ~@(map (fn [c]
                    `(= ~objsym ~c))
                  choices)))))

#_
(mac (in (foo) (bar) (baz)))

(defmacro inq
  "'in queue'. A quoting variant of `in`."
  [obj & args]
  `(in ~obj ~@(map (fn [a]
                     `'~a)
                   args)))

; (inq operator + - *) expands to (in operator '+ '- '*)
#_
(mac
  (inq operator + - *))

; I don't see the use for `in-if` when we already have `some`. But that's
; true for CL true.
(defmacro in-if 
  "Same as `in`, but uses a predicate to test for membership."
  [f & choices]
  (let [fnsym (gensym)]
    `(let [~fnsym ~f]
       (or ~@(map (fn [c]
                    `(~fnsym ~c))
                  choices)))))

#_
(mac (in-if odd? 1 2))

(defn >casex [g [key more :as cl]]
  (cond (coll? key) `[(in ~g ~@key) ~more]
        (boolean key) `[true ~more]
        :else (throw (Exception. "bad >case clause"))))

(defmacro >case 
  "Same as `case`, except the keys guarding each clause are evaluated
  before comparison."
  [expr & clauses]
  (let [g (gensym)]
    `(let [~g ~expr]
       (cond
         ~@(mapcat (fn [cl] (>casex g cl))
                   (partition 2 clauses))))))

#_
(mac
  (>case 2
    [(boolean true) (or true false)] 1
    :else false))

