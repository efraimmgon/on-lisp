(ns on-lisp.chap07
  (:require
    [on-lisp.utils :refer [member]]))

;;;; Macros

;;; ---------------------------------------------------------------------------
;;; 7.1 How Macros Work

(defmacro nil! [var]
  (list reset! var nil))
#_(def a (atom 1))
#_(nil! a)

;;; ---------------------------------------------------------------------------
;;; 7.2 Backquote

(def a 1)
(def b 2)
(def c 3)

`(a ~b c)
`(a (~b c))
`(a b ~c ('~(+ a b c)) (+ a b) 'c '((~a ~b)))

; The general rule is: a comma surrounded by n commas must be surrounded
; by at least n+1 backquotes. An obvious corollary is that commas may not
; appear outside of a backquoted expression.

;; With backbquote:
(defmacro nif
  "The first arg should evaluate to a number. Then the second, third, or
  fourth arg is evaluated, depending on whether the first was positive,
  zero, or negative."
  [expr pos zero neg]
  `(case (Integer/signum ~expr)
     1 ~pos
     0 ~zero
     -1 ~neg))

;; Without backquote:
(defmacro nif [expr pos zero neg]
  (list 'case
        (list 'Integer/signum expr)
        1 pos
        0 zero
        -1 neg))

#_(map #(nif % 'p 'z 'n)
       [0 2.5 -8])

(defmacro our-when [test & body]
  `(if ~test
     (do ~@body)))

;;; ---------------------------------------------------------------------------
;;; 7.3 Defining Simple Macros

; call: (memq x choices)
; expansion: (member x {:test =} choices)

(defmacro memq [obj coll]
  `(member ~obj {:test =} ~coll))

;; call:
; (our-while hungry
;   (stare-intently)
;   (meow)
;   (rub-against-legs))

;; expansion:
; (loop []
;   (when hungry
;     (stare-intently)
;     (meow)
;     (rub-against-legs)
;     (recur)

(defmacro our-while [test & body]
  `(loop []
     (when ~test
       ~@body
       (recur))))

;;; ---------------------------------------------------------------------------
;;; 7.4 Testing Macroexpansion

#_
(clojure.pprint/pprint
  (macroexpand-1
   '
    (our-while hungry
      (stare-intently)
      (meow)
      (rub-against-legs))))

(defmacro mac [expr]
  `(clojure.pprint/pprint (macroexpand-1 '~expr)))

#_
(mac
  (our-while hungry
    (stare-intently)
    (meow)
    (rub-against-legs)))


; Sometimes, though, the expansion will look fine and you’ll want to 
; evaluate it to see where the problems arise. If the expansion contains 
; free variables, you may want to set some variables first. In some systems, 
; you will be able to copy the expansion and paste it into the toplevel, or 
; select it and choose eval from a menu. In the worst case you can set a 
; variable to the list returned by macroexpand-1, then call eval on it:

(def exp (macroexpand-1 '(memq 'a '[a b c])))
(eval exp)

;;; ---------------------------------------------------------------------------
;;; 7.5 Destructuring in Parameter Lists

; (when-bind [input (get-user-input)]
;   (process input))

; (let [input (get-user-input)]
;   (when input
;     (process input)))

(defmacro when-bind [[var expr] & body]
  `(let [~var ~expr]
     (when ~var
       ~@body)))

#_
(when-bind [x (seq (range 2))]
  x)

;;; ---------------------------------------------------------------------------
;;; 7.6 A Model of Macros

(defmacro our-expander
  [name]
  `(:expander (meta ~name)))

;; not sure this is correct
(defmacro our-defmacro
  [name params & body]
  (let [g (gensym)]
    `(do
       (with-meta 
         ~name
         {:expander
          (fn [~g]
            (let [~@(interleave params (rest g))]
              ~@body))}))))

(defn our-macroexpand-1
  [expr]
  (if-let [expander (and (sequential? expr)
                         (our-expander (first expr)))]
    (expander expr)
    expr))

;;; ---------------------------------------------------------------------------
;;; 7.7 Macros as Programs

#_
(mac
  (cl-do 
    [[w 3]
     [x 1 (inc x)]
     [y 2 (inc y)]
     [z]]
    [(> x 10) (prn 'z z) ['y y]]
    (prn 'x x)
    (prn 'y y)))

#_
(loop [w 3
       x 1
       y 2
       z nil]
  (if (> x 10)
    (do
      (prn 'z z)
      ['y y])
    (do
      (prn 'x x)
      (prn 'y y)
      (recur w (inc x) (inc y) z))))

#_
(make-initforms
  '[[w 3]
    [x 1 (inc x)]
    [y 2 (inc y)]
    [z]])
#_
(make-stepforms
  '[[w 3]
    [x 1 (inc x)]
    [y 2 (inc y)]
    [z]])


(defn make-initforms [bindforms]
  (reduce (fn [acc [name val _]]
            (conj acc name val))
          [] bindforms))

(defn make-stepforms
  [bindforms]
  (map (fn [[name _ step :as b]]
         (if (= 3 (count b))
           step
           name))
       bindforms))

(defmacro cl-do
  [bindforms [test & result] & body]
  `(loop ~(make-initforms bindforms)
     (if ~test
       (do ~@result)
       (do
         ~@body
         (recur ~@(make-stepforms bindforms))))))


;;; ---------------------------------------------------------------------------
;;; 7.8 Macro Style

(defmacro our-and [& args]
  (case (count args)
    0 true
    1 (first args)
    true `(if ~(first args)
            (our-and ~@(rest args)))))

(defmacro our-and [& args]
  (if (empty? args)
    true
    (letfn [(expander [args*]
              (if (next args*)
                `(when ~(first args*)
                   ~(expander (rest args)))
                (first args*)))]
      (expander args))))

;;; ---------------------------------------------------------------------------
;;; 7.10 Macros from Functions

; The easiest class to translate are the functions which:

; 1. Have a body consisting of a single expression.
(defn our-second [x] (first (rest x)))

; Simply put a backquote in front of the body and a comman in front of each 
; symbol which occurs in the parameter list:
(defmacro our-second [x] `(first (rest ~x)))

; The technique changes slightly when the body has more than one expression,
; because a macro must expand into a single expression. You have to add
; a `do`:
(defn noisy-second [x]
  (println "Someone is taking a cadr!")
  (first (rest x)))

(defmacro noisy-second [x]
  `(do
     (println "Someone is taking a cadr!")
     (first (rest ~x))))


; 2. Have a parameter list consisting only of parameter names.
(defn sum [& args]
  (apply + args))

; When the functions doesn't meet condition 2 because it has an & parameter,
; the rules are the same, except that the parameter, instead of simply
; having a comma before it, must be spliced into a call of `list`:
(defmacro sum [& args]
  `(apply + (list ~@args)))

; Which in this case would be better rewritten
(defmacro sum [& args]
  `(+ ~@args))


; 3. Create no new variables (except the parameters).

; When condition 3 doesn't, hold the rule about the insertion of commas
; must be modified. Instead of putting commas before all symbols in the 
; parameter list, we only put them before those which will refer to the
; parameters.

; In the next function neither of the last two instances of x will refer
; to the parameter x. The second instance is not evaluated at all, and the
; third instance refers to a new variable established by the `let`. So
; only the first instance will get a comma.

(defn foo [x y z]
  (list x (let [x y]
            (list x z))))

(defmacro foo [x y z]
  (list ~x (let [x ~y]
             (list x ~z))))


; 4. Are not recursive (nor part of a mutually recursive group).

; Check chap. 10.4

; 5. Have no parameter which occurs more than once in the body.

; Check chap. 10.1

; 6. Have no parameter whose value is used before that of another parameter
; occurring before it in the parameter list.

; Check chap. 10.2

; 7. Contain no free variables.
