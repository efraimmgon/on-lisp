(ns on-lisp.chap03)

;;;; --------------------------------------------------------------------------
;;;; 3 FUNCTIONAL PROGRAMMING

(defn good-reverse [coll]
  (letfn [(rev [coll acc]
            (if-not (seq coll)
              acc
              (recur (rest coll) (conj acc (first coll)))))]
    (rev coll nil)))

;; Using reduce we can achieve a shorter and more simpler code.
(defn good-reverse [coll]
  (reduce conj nil coll))
#_(good-reverse '[a b c])