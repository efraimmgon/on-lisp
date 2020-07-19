(ns on-lisp.chap22
  (:require
    [on-lisp.utils :refer [mac]]
    [on-lisp.chap20 :refer [=defn =values =bind cont]]))

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

;;; Figure 22.5: Nondeterministica operators in Common Lisp

(def paths
  "Paths which have not yet been followed."
  (atom nil))

(def failsym ::fail)
  
(defn fail 
  "Pops the last stored choice, and restarted. When there are no more paths
  left to restart, returns a `::fail`."
  []
  (if (seq @paths)
    (let [p (peek @paths)]
      (swap! paths pop)
      (p))
    failsym))

(defn cb [f choices]
  (if (seq choices)
    (do
      (when-let [more (next choices)]
        (swap! paths conj #(cb f more)))
      (f (first choices)))
    (fail)))
  
(defmacro choose-bind
  "Takes a symbol, a list of choices, and a body of code. It will do a
  `choose` on the choices, bind the symbol the the value chosen, and 
  evaluate the body of code."
  [var choices & body]
  `(cb (fn [~var] ~@body) ~choices))

(defmacro choose 
  "Taks any number of expressions, from which it chooses one to evaluate."
  [& choices]
  (if (seq choices)
    `(do
       ~@(map (fn [c]
                `(swap! paths conj (fn [] ~c)))
              (reverse (rest choices)))
       ~(first choices))
    `(fail)))


(comment
  "It is only for convenience that the Common Lisp implementation provides
  two choice operators. You could get the effect of choose from choose-bind
  by always translating"
  (choose (foo) (bar))
  "into"
  (choose-bind x '(1 2)
    (case x
      1 (foo)
      2 (bar)))
  "but programs are easier to read if we have a separate operator for this case.")

(comment
  (defn do2 [x]
    (choose (+ x 2) (* x 2) (Math/pow x 2)))
  
  (do2 3)
  (fail)

  (choose-bind x '[makkaresh strasbourg vegas]
               (print "Let's go to" x))
  (fail))

;;; Figure 22.6: Common Lisp choice in a subroutine

(=defn two-numbers []
  (choose-bind n1 (range 6)
    (choose-bind n2 (range 6)
      (=values n1 n2))))

(=defn parlor-trick [sum]
  (=bind [n1 n2] (two-numbers)
    (if (= (+ n1 n2) sum)
      `("the sum of" ~n1 ~n2)
      (fail))))

(comment
  (parlor-trick 7))

(comment
  "The restrictions on the use of choose, choose-bind, and fail are the
  same as the restrictions given in figure 20.5 for code which uses the
  continuation-passing-macros. Where a choice expression occurs, it must
  be the last thing to be evaluated. Thus if we want to make sequential
  choices, in Common Lisp the choices have to be nested:"
  (choose-bind first-name [:henry :william]
    (choose-bind last-name [:james :higgins]
      (=values (list first-name last-name))))
  (fail))

;;; Figure 22.7: Nondeterministic search in Common Lisp

(defn kids [n]
  (case n
    a '(b c)
    b '(d e)
    c '(d f)
    f '(g)
    nil))

(=defn descent [n1 n2]
  (cond (= n1 n2) (=values (list n2))
        (kids n1) (choose-bind n (kids n1)
                    (=bind [p] (descent n n2)
                      (=values (cons n1 p))))
        :else (fail)))

(comment
  (descent 'a 'g)
  (fail)
  (descent 'a 'd)
  (fail)
  (fail)
  (descent 'a 'h))