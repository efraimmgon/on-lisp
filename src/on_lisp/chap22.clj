(ns on-lisp.chap22
  (:require
    [on-lisp.utils :refer [mac]]))

;;; 22 - Nondeterminism

;;; ---------------------------------------------------------------------------
;;; 22.1 The Concept

(comment
  "If `choose` is given a choice of several alternatives, each one is 
  associated with a set of possible futures.
  
  We can assume that choose works as follows:
  
  1. It will only return a choice for which some future does not contain a 
  call to fail.
  
  2. A choose over zero alternatives is equivalent to a fail")
  
  
;;; ---------------------------------------------------------------------------
;;; 22.2 Search

(comment
  "Figure 22.1: Deterministic tree search."

  (declare descent)

  (defn try-paths [ns n2]
    (when (seq ns)
      (or (descent (first ns) n2)
          (recur (rest ns) n2))))
      
  (defn descent [n1 n2]
    (if (= n1 n2)
      (list n2)
      (let [p (try-paths (kids n1) n2)]
        (when p 
          (cons n1 p))))))

(comment
  "Figure 22.2 Nondeterministic tree search"
  
  (defn descent [n1 n2]
    (cond (= n1 n2) (list n2)
          (empty? (kids n1)) (fail)
          :else (cons n1 (descent (choose (kids n1)) n2)))))

(comment
  "Figure 22.3: Choice in a subroutine"
  
  (defn two-numbers []
    [(choose (range 6))
     (choose (range 6))])

  (defn parlor-trick [sum]
    (let [nums (two-numbers)]
      (if (= (apply + nums) sum)
        (str "the sum of " nums)
        (fail)))))

;;; ---------------------------------------------------------------------------
;;; 22.4 Common Lisp Implementation


(def paths
  "Paths which have not yet been followed."
  nil)
  
(def choose-bind
  "Takes a symbol, a list of choices, and a body of code. It will do a
  `choose` on the choices, bind the symbol the the value chosen, and 
  evaluate the body of code.")