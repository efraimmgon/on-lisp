(ns on-lisp.chap01)

;;; 1.4 Extending Lisp

(defn map1-n 
  "Returns a list of the values returned by some function when it is
  applied to all the integers from 1 to 10."
  [f]
  (->> (range 1 11)
       (map f)))
