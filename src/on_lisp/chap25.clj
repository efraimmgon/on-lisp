(ns on-lisp.chap25
  (:require
    [on-lisp.utils :refer [mac cl-atom]]))

;;; 25 Prolog

;;; ---------------------------------------------------------------------------
;;; 25.2 Objects in Plain Lisp

(comment
  "Object-oriented programming, at a minimum, implies
  
  1. objects which have properties
  
  2. and respond to messages,
  
  3. and which inherit properties and methods from their parents.")

(defn tell [obj message & args]
  (apply (obj message) obj args))
#_(tell obj :move 10)

(defn rget [obj prop]
  (if-let [val (obj prop)]
    val
    (let [par (obj :parent)]
      (and par (rget par prop)))))


;;; Figure 25.1: Multiple inheritance

(comment
  "We can have multiple inheritance by making the `parent` property a list,
  and defining `rget` as in figure 25.1."
  
  "If we want to implement the usual idea of inheritance, we should never 
  examine an object before one of its descendants. The simplest way is to
  assemble a list of all the ancestors of the original object, sort the list 
  so that no object appears before one of its descendants, and then look at
  each element in turn.")

(defn get-ancestors 
  "Returns a properly ordered list of an object and its ancestors."
  [obj]
  (letfn [(getall [x]
            (concat (list x)
                    (mapcat getall (:parents x))))]
    (sort-by 
      :parents
      (set (getall obj)))))

(defn rget 
  "Searches for the first object with the desired property."
  [obj prop]
  (some #(get % prop)
        (get-ancestors obj)))

(comment
  (let [scoundrel {:serves :self}
        patriot {:serves :country}
        patriotic-scoundrel {:parents [scoundrel patriot]}]
    (rget patriotic-scoundrel :serves)))


;;; Figure 25.3_ A function to create objects.
      
(defn obj 
  "Creates a new object, storing within it a list of its ancestors."
  [& parents]
  (let [obj {:parents parents}]
    (if (:ancestors obj)
      obj
      (assoc obj :ancestors (get-ancestors obj)))))
    

(defn rget
  [obj prop]
  (some #(get % prop)
        (:ancestors obj)))


(comment
   "Figure 25.4: Functional syntax.")
   
(defn run-methods [obj name args]
  (let [meth (rget obj name)]
    (if meth
      (apply meth obj args)
      (throw (str "No " name " method for " obj ".")))))

(comment
  (let [scoundrel (-> (obj) (assoc :serves 'self))
        patriot (-> (obj) (assoc :serves 'country))
        patriotic-scoundrel (obj scoundrel patriot)]
    (rget patriotic-scoundrel :serves)))


;;; Figure 25.5 and 25.6

(comment
  (let [rectangle (-> (obj)
                      (assoc :area #(* (:height %) (:width %))))
        myrec (-> (obj rectangle)
                  (assoc :height 2, :width 3))]
    ((rget myrec :area) myrec)))
