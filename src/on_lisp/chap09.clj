(ns on-lisp.chap09
  (:require
    [on-lisp.utils :refer [mac]]))

;;;; Variable Capture

;;; ---------------------------------------------------------------------------
;;; 9.1 Macro Argument Capture


;; Wrong. `limit` fucks things up
(defmacro our-for [[var start stop] & body]
  `(let [~'limit ~stop]
     (loop [~var ~start]
       (when-not (> ~var ~'limit)
         ~@body
         (recur (inc ~var))))))
#_
(mac
  (our-for [x 1 5]
    (pr x)))
#_
(mac
  (our-for [limit 1 5]
    (pr limit)))

;;; ---------------------------------------------------------------------------
;;; 9.2 Free Symbol Capture

(def w (atom nil))

;; wrong
(defmacro gripe [warning]
  `(do (swap! w conj ~warning)
       nil))

;; This example of free symbol capture does not work with clojure since
;; in `grip` `w` will be resolved to a fully qualified symbol, and will
;; still refer to `on-lisp.chap9/w`.
(defn sample-ratio [v w]
  (let [vn (count v)
        wn (count w)]
    (if (or (< vn 2) (< wn 2))
      (gripe "sample < 2")
      (/ vn wn))))
#_
(let [lst '[b]]
  (sample-ratio nil lst)
  lst)
#_(deref w)

;;; ---------------------------------------------------------------------------
;;; 9.3 When Capture Occurs

; Free: A symbol s occurs free in an expression when it is used as a variable
; in that expression, but the expression does not create a binding for it.

; In the following expression,
#_
(let [x y
      z 10]
  (list w x z))
; w, x and z all occur free within the list expression, which establishes 
; no bindings. However, the enclosing let expression establishes bindings 
; for x and z, so within the let as a whole, only y and w occur free.


; Skeleton: the skeleton of a macro expansion is the whole expansion, 
; minus anything which was part of an argument in the macro call.

; If foo is defined:
#_
(defmacro foo [x y]
  `(/ (+ ~x 1) ~y))
; and called thus:
#_(foo (- 5 2) 6)
; then it yields the macro expansion:
#_(/ (+ (- 5 2) 1) 6)
; The skeleton of this expansion is the above expression with holes where
; the parameters x and y got inserted.
#_(/ (+ _ 1) _)


; Capturable: A symbol is capturable in some macro expansion if (a) it
; occurs free in the skeleton of the macro expansion, or (b) it is bound
; by a part of the skeleton in which arguments passed to the macro are 
; either bound or evaluated.

; This example of variable capture doesn't occurs with clojure, since x
; will get resolved to a fully qualified var.
(defmacro cap1 []
  `(+ x 1))

(defmacro cap1-1 []
  `(+ ~'x 1))
#_(mac (cap1-1))

; In this example x is supposed to be sucestible to variable capture (because 
; it is bound in an expression where an argument to the macro call will also 
; be bound), but not in clojure.
; In fact, we'll get an error because x wil be resolved to
; a fully qualified var, which let does not accept.
(defmacro cap2 [var]
 `(let [x _
        ~var _]
    _))

(defmacro cap2-2 [var]
  `(let [~'x :x
         ~var :var]
     (list ~'x ~var)))
#_(mac (cap2-2 x))
         

; Same as before; will throw an error on x.
(defmacro cap3 [var]
  `(let [x _]
     (let [~var _]
       _)))

(defmacro cap3-3 [var]
  `(let [~'x :x]
     (let [~var :var]
       (list ~'x ~var))))
#_(mac (cap3-3 x))

; Same as before; will throw an error on x.
(defmacro cap4 [var]
  `(let [~var _]
     (let [x _]
       _)))

(defmacro cap4-4 [var]
  `(let [~var :var]
     (let [~'x :x]
       (list ~var ~'x))))
#_(mac (cap4-4 x))

; However, if there is no context in which the binding of x and the 
; variable passed as an argument will both be visible, as in
; (Same as before; will throw an error on x.)
(defmacro safe1 [var]
  `(do
     (let [x 1]
       (pr x))
     (let [~var 1]
       (pr ~var))))

(defmacro safe1 [var]
  `(do
     (let [~'x 1]
       (pr ~'x))
     (let [~var 1]
       (pr ~var))))
; then x won't be capturable.

; However, if arguments to the macro call are evaluated within a binding
; established by the skeleton,
(defmacro cap5 [& body]
  `(let [~'x :x]
     ~@body))
; then variables so bound are at risk of capture: in cap5, x is capturable.
; In this case, though,
(defmacro safe2 [expr]
  `(let [~'x ~expr]
     (cons ~'x 1)))
; x is not capturable, because when the argument passed to expr is evaluated,
; the new binding of x won't be visible. Note also that it's only the binding
; of skeletal variables we have to worry about.
; In this macro
(defmacro safe3 [var & body]
  `(let [~var :var]
     ~@body))
; no symbol is at risk of inadvertent capture (assuming that the user 
; expects that the first argument will be bound).

; The reules presented in this section should be used with the reservation
; that they are intended only as a guide. they are not even formally stated,
; let alone formally correct. The problem of capture is a vaguely defined
; one, since it depends on expectations.
; The rules for detecting capture are also imprecise. You could write macros
; which passed these tests, and which still would be vulnerable to unintended
; capture. For example,
(defmacro pathological [& body]
  (let [syms (filter symbol? (flatten body))
        var (rand-nth syms)]
    `(let [~var 99]
       ~@body)))
; When this macro is called, the expressions in the body will be evaluated as
; if in a do - but one random variable within the body may have a different
; value. This is clearly capture, but it passes our tests, because the
; variable does not occur in the skeleton.


;;; ---------------------------------------------------------------------------
;;; 9.4 Avoiding Capture with Better Names

; pg talks about the convention of using * in the beginning and end of
; global names. In clojure that's not a source of concern since the
; functional paradigm is the rule and you can't rebind names the way
; you do it with CL.


;;; ---------------------------------------------------------------------------
;;; 9.5 Avoiding Capture by Prior Evaluation

; Vulnerable to capture:
(defmacro before-0 [x y coll]
  `(let [~'coll ~coll]
     (> (count (drop-while #(not= ~x %) ~'coll))
        (count (drop-while #(not= ~y %) ~'coll)))))

; A correct version:
(defmacro before-1 [x y coll]
  `(let [~'xval ~x
         ~'yval ~y
         ~'coll ~coll]
     (> (count (drop-while #(not= ~'xval %) ~'coll))
        (count (drop-while #(not= ~'yval %) ~'coll)))))

; Note: the example given does not happen in clojure, due to its
; immutability. (In fact, it seems most sections in this chapter do not apply
; to clojure, simply due to its immutable data structures and vars being
; fully qualified in macros.)
; (before (progn (setq seq '(b a) 'a))
;         'b
;         '(a b))

; Unfortunately, the let technique works only in a narrow range of cases:
; macros where
; 1. all the arguments at risk of capture are evaluated exactly once, and
; 2. none of the arguments need to be avaluated in the scope of bindings
; established by the macro skeleton.

; Vulnerable to capture:
(defmacro our-for-1 [[var start stop] & body]
  `(let [~'limit ~stop]
     (loop [~var ~start]
       (when-not (> ~var ~'limit)
         ~@body
         (recur (inc ~var))))))

; A correct version
(defmacro our-for-2 [[var start stop] & body]
  `(let [~'b (fn [~var] ~@body)
         ~'limit ~stop]
     (loop [~'counter ~start]
       (when-not (> ~'counter ~'limit)
         (~'b counter)
         (recur (inc ~'counter))))))
; Since the closure is the first thing made by the expansion of a for, 
; free synbols occurring in the body will all refer to variables in the
; environment of the macro call. Now the loop comunicates with its body
; through the parameters of the closure. All the closure needs to know
; from the loop is the number of the current iteration, so it has only
; one parameter, the symbol specified as the index variable in the macro call.

;;; ---------------------------------------------------------------------------
;;; 9.6 Avoiding Capture with Gensyms

(defmacro our-for [[var start stop] & body]
  `(let [gstop# ~stop]
     (loop [~var ~start]
        (when-not (> ~var gstop#)
          ~@body
          (recur (inc ~var))))))

#_
(mac
  (our-for [x 0 5]
           (prn x)))
