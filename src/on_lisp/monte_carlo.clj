(ns on-lisp.monte-carlo)

; - If a person tried russion roulette, how many successful tries would
; he have, on average?

; - calculate n of tries
(defn simulation [{:keys [holes n]}]
  (loop [n n
         tries 1
         acc []]
    (cond 
      ;; Total of runs is over
      (zero? n) 
      acc
      
      ;; User has shot himself in the head
      (zero? (rand-int holes))
      (recur (dec n) 1 (conj acc tries))
      
      ;; Lucky! Try again
      :else
      (recur n (inc tries) acc))))
  
#_(def ret
    (simulation
      {:holes 5
       :n 1000000}))

;; avg
#_
(float (/ (apply + ret)
          (count ret)))

;; most tries?
#_(apply max ret)

;; less tries? (though the result is not surprising)
#_(apply min ret)

;; histogram
#_
(clojure.pprint/pprint 
  (sort-by first (frequencies ret)))

(defn normalize
  "Multiply each number by a constant such that the sum is 1.0 (or total).
  >>> (normalize [1, 2, 1])
  [0.25, 0.5, 0.25]
  "
  ([numbers] 
   (normalize numbers 1.0))
  ([numbers total]
   (let [k (float (/ total (apply + numbers)))]
     (for [n numbers]
       (* k n)))))

;; normalize
#_(clojure.pprint/pprint 
    (normalize ret 1000))