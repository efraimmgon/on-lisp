(ns on-lisp.chap10
  (:require
    [on-lisp.utils :refer [mac]]))

;;;; 10 Other Macro Pitfalls

;;; ---------------------------------------------------------------------------
;;; 10.1 Number of Evaluations

;; A correct version:
(defmacro our-for [[var start stop] & body]
  `(loop [~var ~start
          gstop# ~stop]
     (when-not (> ~var gstop#)
       ~@body
       (recur (inc ~var) gstop#))))

;; Subject to multiple evaluations:
#_
(defmacro our-for [[var start stop] & body]
  `(loop [~var ~start]
     (when-not (> ~var ~stop)
       ~@body
       (recur (inc ~var)))))

;; Incorrect order of evaluation:
#_
(defmacro our-for [[var start stop] & body]
  `(loop [gstop# ~stop
          ~var ~start]
     (when-not (> ~var gstop#)
       ~@body
       (recur gstop# (inc ~var)))))

; This loop will never terminate, because the goal recedes on each iteration:
#_(let [x (atom 2)]
    (our-for [i 1 (swap! x inc)]
      (pr i)))


;;; ---------------------------------------------------------------------------
;;; 10.2 Order of Evaluation

;; The third version of for contains a sutble bug. The parameter `stop`
;; will be evaluated before `start`, even though they appear in the 
;; opposite order in the macro cal. The evaluation of the `stop` form 
;; influences the value returned by the `start` form, even though the
;; `start` form appears first textually.
#_
(let [x (atom 1)]
  (our-for [i @x (reset! x 13)]
    (pr i)))


;;; ---------------------------------------------------------------------------
;;; 10.3 Non-functional Expanders

; Note: this is avoided in clojure because of the immutable data structures.
(defn et-al [& args]
  (concat args '[et al]))

#_
(et-al 'smith 'jones)

;;; ---------------------------------------------------------------------------
;;; 10.4 Recursion

;;; Figure 10.2 - Mistaken analogy to a recursive function.

; This will work:
(defn ntha [n lst]
  (if (zero? n)
    (first lst)
    (recur (dec n) (rest lst))))

; This won't compile:
(defmacro nthb [n lst]
  `(if (zero? ~n)
     (first ~lst)
     (nthb (dec ~n) (rest ~lst))))


;;; Figure 10.3 - Two ways to fix the problem.

(defn nth-fn [n lst]
  (if (zero? n)
    (first lst)
    (recur (dec n) (rest lst))))

(defmacro nthd [n lst]
  `(nth-fn ~n ~lst))

(defn nthe [n lst]
  `(letfn [(nth-fn [n lst]
             (if (zero? n)
               (first lst)
               (recur (dec n) (rest lst))))]
     (nth-fn ~n ~lst)))


;;; Figure 10.4 - Recursive expansion functions.

(defn or-expand [args]
  (when (seq args)
    `(let [sym# ~(first args)]
       (if sym#
         sym#
         ~(or-expand (rest args))))))
         
(defmacro ora [& args]
  (or-expand args))

(defmacro orb [& args]
  (when (seq args)
    `(let [sym# ~(first args)]
       (if sym#
         sym#
         (orb ~@(rest args))))))

#_
(mac
  (orb 1 2 3 4))