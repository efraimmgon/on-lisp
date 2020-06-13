(ns on-lisp.chap04
  (:require
    [on-lisp.utils :refer [member mkcoll]]))
;;;; --------------------------------------------------------------------------
;;;; 4 UTILITY FUNCTIONS

;;; ---------------------------------------------------------------------------
;;; 4.1 Birth of a Utility

(defn bookshops [town])

(defn find-books [towns]
  (when (seq towns)
    (if-let [shops (bookshops (first towns))]
      [(first towns) shops]
      (recur (rest towns)))))

(defn find2
  "O(n). Takes a function and a coll, applying f to each element of coll.
  If the function returns a truthy value, both the successful element and
  the value returned by the test function are returned."
  [f coll]
  (when (seq coll)
    (if-let [val (f (first coll))]
      [(first coll) val]
      (recur f (rest coll)))))

#_(find2 bookshops towns)


;;; ---------------------------------------------------------------------------
;;; 4.3 Operation on Lists

; `last1` is already implemented on `last`

(defn single 
  "Returns true if the collection has only ony item."
  [coll]
  (and (coll? coll)
       (empty? (rest coll))))

; `append1` isn't necessary in clojure, since we can simply conj an item
; into a vector.
; (defn append1 [obj coll]
;   (concat coll [obj]))


;; In clojure getting the length of a data structure is O(1). 
;; But `longerp  is useful in the context of lazy structures, since to get
;; its length we'd need to realize it. So that must be taken into account.

(defn longer 
  "Compares two colls and true if the first is longer."
  [x y]
  (letfn [(compare* [x y]
            (and (seq x)
                 (or (empty? y)
                     (recur (rest x) (rest y)))))]
    (if (or (instance? clojure.lang.LazySeq x) 
            (instance? clojure.lang.LazySeq y))
      (compare* x y)
      (> (count x) (count y)))))
      
; `filter` is already implemented.
; `group` is already implemented, in `partition
; `flatten` is already implemented.
(defn flatten* [x]
  (letfn [(rec [x acc]
            (cond 
              (and (coll? x) (empty? x)) acc
              (not (sequential? x)) (conj acc x)
              :else (recur (first x)
                           (rec (rest x) acc))))]
    (rec x nil)))
#_(flatten* [1 2 [3 4] 5 [6 [7]]])

; - `prune` is to `remove` as copy-tree is to copy-list. That is,
; it recurses down into sublists.
; - Every lear for which the function returns true is removed.
(defn prune [f tree]
  (letfn [(rec [tree acc]
            (cond
              (empty? tree) 
              acc
              
              (coll? (first tree))
              (rec (rest tree)
                   (conj acc (rec (first tree) [])))
              
              :else 
              (rec (rest tree)
                   (if (f (first tree))
                     acc
                     (conj acc (first tree))))))]
    (rec tree [])))
#_(prune even? [1 2 [3 [4 5] 6] 7 8 [9]])

;;; ---------------------------------------------------------------------------
;;; 4.4 Search

(defn find2
  "O(n). Takes a function and a coll, applying f to each element of coll.
  If the function returns a truthy value, both the successful element and
  the value returned by the test function are returned."
  [f coll]
  (when (seq coll)
    (if-let [val (f (first coll))]
      [(first coll) val]
      (recur f (rest coll)))))

(defn before
  "Takes two objects and tells you if one is found before another in a sequence.
  `f` is a function of two args."
  ([x y coll]
   (before x y = coll))
  ([x y f coll]
   (and (seq coll)
        (let [first* (first coll)]
          (cond
            (f y first*) nil
            (f x first*) coll
            :else (recur x y f (rest coll)))))))
#_(before 'b 'd '[a b c d])
#_(before 'a 'b '[a])

(defn after
  "Takes two objects and tells you if one is found after another in a sequence.
  `f`is a function of two args."
  ([x y coll]
   (after x y = coll))
  ([x y f coll]
   (let [ret (before y x f coll)]
     (and ret
          (member x f ret)))))
#_(after 'a 'b '[b a d])
#_(after 'a 'b '[a])

(defn duplicate
  "Tests if there's (at least) a duplication of x in coll."
  ([x coll]
   (duplicate x = coll))
  ([x f coll]
   (->> (member x f coll)
        rest
        (member x f))))
#_(duplicate 'a '[a b c a d])

; `split-if` is kind of implemented in `split-with`, although in a different
; way.
(defn split-if
  "Returns both halves of the coll beginning with the element it finds."
  [f coll]
  (loop [remain coll
         acc []]
    (if (or (empty? remain) 
            (f (first remain)))
      [acc remain]
      (recur (rest remain) 
             (conj acc (first remain))))))
#_(split-if #(> % 4) (range 1 11))
#_(split-with #(<= % 4) (range 1 11))

(defn most
  "Takes a coll and a scoring function and returns the elements with the
  highest score. In case of ties, the element occurring first wins.
  Also returns the score of the winner"
  [f coll]
  (if (empty? coll)
    [nil nil]
    (->> coll
         (map (fn [x] [x (f x)]))
         (reduce (fn [[wins max :as acc0] [x score :as acc1]]
                   (if (> score max)
                     acc1 acc0))))))
#_(most count '[[a b] [a b c] [a] [e f g]])

(defn best
  "Takes a function of two args and a coll. Returns the element that
  beats all the others.
  Equivalent fo `first` of `sort`, but much more efficient.
  In case of ties the first element wins."
  [f coll]
  (when (seq coll)
    (reduce (fn [wins x]
              (if (f x wins)
                x wins))
            coll)))
#_(best > (range 1 6))

(defn mostn
  "Returns a vector of all the elements for which the function yields
  the highest score (along with the score itself)."
  [f coll]
  (if (empty? coll)
    [nil nil]
    (->> (rest coll)
         (map (fn [x] [x (f x)]))
         (reduce (fn [[result max :as acc0] [x score :as acc1]]
                   (cond
                     (> score max) [[x] score]
                     (= score max) [(conj result x) max]
                     :else acc0))
                 [[(first coll)] (f (first coll))]))))
#_(mostn count '[[a b] [a b c] [a] [e f g]])

;;; ---------------------------------------------------------------------------
;;; 4.5 Mapping


(defn map0-n [f n]
  (map f (range (inc n))))
#_(range 1 7)
#_(map0-n inc 5)
; => [1 2 3 4 5 6]

(defn map1-n [f n]
  (map f (range 1 (inc n))))

(defn mapa-b 
  ([f a b]
   (mapa-b f a b 1))
  ([f a b step]
   (map f
        (range a (+ b step) step))))
#_(range -1 1.5 0.5)
#_(mapa-b inc -2 0 0.5)
; => [-1 -0.5 0.0 0.5 1.0]

(defn map-> 
  "The sequence begins with the object given as the second argument, the 
  end of the sequence is defined by testf, and successors are generated
  by succf"
  [f start testf succf]
  (loop [i start
         result []]
    (if (testf i)
      result
      (recur (succf i) 
             (conj result (f i))))))
#_(map-> inc -2 #(> % 0) #(+ % 0.5))

; `mappend` is already implemented with `mapcat`

; `mapcars` is unnecessary in clojure. Just use map and concat/into

(defn rmap
  "Recursive map. What map does on flat colls it does on tree."
  [f & colls]
  (if (some #(or (not (coll? %)) (empty? %)) colls)
    (apply f colls)
    (apply map
           (fn [& args]
             (apply rmap f args))
           colls)))
#_(rmap + [1 [2 [3] 4]] [10 [20 [30] 40]])

;;; ---------------------------------------------------------------------------
;;; 4.6 I/O

(defn readlist 
  "Reads a line of input and returns it as a list."
  [& args]
  (read-string
    (str "("  
         (apply read-line args)
         ")")))
#_(readlist) ; Call me "Ed"

(defn prompt
  "Combines printing a question and reading the answer."
  [& args]
  (apply printf args)
  (flush)
  (clojure.edn/read))
#_(prompt "Enter a number between %s and %s.%n" 1 10)

(defn break-loop
  "For situations where you want to imitate the Lisp toplevel.
  Takes two functions and a &rest argument, which is repeatedly given
  to `prompt`. As long as the second function returns false for the 
  input, the first function is applied to it."
  [f quit & args]
  (println "Entering break-loop.")
  (loop []
    (let [in (apply prompt args)]
      (when-not (quit in)
        (printf (str (f in))) 
        (newline) (flush)
        (recur)))))
#_(break-loop eval #(= % :q) ">> ") ; (+ 2 3); :q

;;; ---------------------------------------------------------------------------
;;; 4.7 Symbols and Strings

; `mkstr` is already implemented with `str`
#_(str Math/PI " pieces of " 'pi)

; `symb` is unnecessary. Just use `symbol` with `str`
#_(symbol (str 'ar "Madi" \L \L 0))

(defn explode 
  "Takes a symbol and returns a coll of symbols made from the chars
  in its name."
  [sym]
  (map (comp symbol str)
       (name sym)))
#_(explode 'bomb)