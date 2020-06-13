(ns on-lisp.chap02)

;;;; --------------------------------------------------------------------------
;;;; 2 FUNCTIONS

;;; 2.3 Functional Arguments

(defn our-remove-if [f coll]
  (lazy-seq
    (cond 
      (empty? coll) nil
      (f (first coll)) (our-remove-if f (rest coll))
      :else (cons (first coll)
                  (our-remove-if f (rest coll))))))
#_(our-remove-if even? (range 10))

;;; 2.4 Functions as Properties

(defn behave [animal]
  ((:behavior animal)))
#_(let [dog {:behavior #(println 'wag-tail 'bark)}]
    (behave dog))

;;; 2.6 Closures

(defn make-adder [n]
  (fn [x] (+ x n)))

(defn make-adderb [n]
  (let [n (atom n)]
    (fn [x & [change?]]
      (if change?
        (reset! n x)
        (+ x @n)))))
#_(let [addx (make-adderb 1)]
    (addx 3)
    (addx 100 true)
    (addx 3))

;;; Factorial

(defn fact [f n]
  (if (zero? n)
    1
    (* n (f f (dec n)))))

(defn recurser [f]
  (fn [& args]
    (apply f f args)))
#_((recurser fact) 8)

(defn count-instances [obj colls]
  (letfn [(instances-in [coll]
            (if (seq coll)
              (+ (if (= (first coll) obj) 1 0)
                 (instances-in (rest coll)))
              0))]
    (map instances-in colls)))

;; With clj fn can have a name to become recursive.
(defn count-instances [obj colls]
  (map (fn instances-in [coll]
         (if (seq coll)
           (+ (if (= (first coll) obj) 1 0)
              (instances-in (rest coll)))
           0))
       colls))
#_(count-instances 'a '[[a b c] [d a r p a] [d a r] [a a]])

;;; 2.8 Tail-recursion

(defn our-find-if [f [x & xs :as coll]]
  (when (seq coll)
    (if (f x)
      x
      (recur f xs))))

;; Transforming a function that isn't tail-recursive into one that is
; by embedding in it a local function that uses an accumulator.
(defn our-length [coll]
  (letfn [(rec [coll acc]
            (if-not (seq coll)
              acc
              (recur (rest coll) (inc acc))))]
    (rec coll 0)))
#_(our-length (range 10))

;; Optimization in clojure can be done with type hints.
(defn triangle [^Long n]
  (letfn [(tri [c n]
            (if (zero? n)
              c
              (recur (+ n c)
                     (dec n))))]
    (tri 0 n)))
#_(triangle 10)
