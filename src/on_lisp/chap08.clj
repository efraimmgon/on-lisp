(ns on-lisp.chap08
  (:require
    [on-lisp.utils :refer [mac]]))

;;;; When to Use Macros

;;; ---------------------------------------------------------------------------
;;; 8.1 When Nothing Else Will Do

; 1. Transformations

; 2. Binding

; 3. Conditional evaluation

; 4. Multiple evaluations

; 5. Using the calling environment

; 6. Wrapping a new environment

; 7. Saving function calls


;;; ---------------------------------------------------------------------------
;;; 8.2 Macro or Function?

; The Pros

; 1. Computation at compile-time

; 2. Integration with Lisp

; 3. Saving function calls


; The Cons

; 4. Functions are data

; 5. Clarity of source code

; 6. Clarity at runtime

; 7. Recursion

;;; ---------------------------------------------------------------------------
;;; 8.3 Applications for Macros

(defmacro our-defn [name params & body]
  `(do
     (def ~name
       (fn ~params
         ~@body))
     ~name))

#_
(our-defn plus1 [x] (inc x))


;;; Figure 8.1: Original move and scale.

(defn redraw [min-x min-y max-x max-y])

(defn bounds
  "Returns four coordinates (min x, min y, max x, max y) representing
  the bounding rectangle of a group of objects."
  [objs])

(defn move-objs
  "Slides a group of objects"
  [objs dx dy]
  (let [[x0 y0 x1 y1] (bounds objs)]
    (doseq [o objs]
      (swap! o update :x + dx)
      (swap! o update :y + dy))
    (let [[xa ya xb yb] (bounds objs)]
      (redraw (min x0 xa) (min y0 ya)
              (max x1 xb) (max y1 yb)))))


(defn scale-objs
  "Changes the size of a group of objects."
  [objs factor]
  (let [[x0 y0 x1 y1] (bounds objs)]
    (doseq [o objs]
      (swap! o update :dx * factor)
      (swap! o update :dy * factor))
    (let [[xa ya xb yb] (bounds objs)]
      (redraw (min x0 xa) (min y0 ya)
              (max x1 xb) (max y1 yb)))))


;;; Figure 8.2: Move and scale filleted

(defmacro with-redraw [[var objs] & body]
  `(let [gob# ~objs
         [x0# y0# x1# y1#] (bounds gob#)]
     (doseq [~var gob#] ~@body)
     (let [[xa# ya# xb# yb#] (bounds gob#)]
       (redraw (min x0# xa#) (min y0# ya#)
               (max x1# xb#) (max y1# yb#)))))

(defn move-objs [objs dx dy]
  (with-redraw [o objs]
    (swap! o update :x + dx)
    (swap! o update :y + dy)))


(defn scale-objs [objs factor]
  (with-redraw [o objs]
    (swap! o update :dx * factor)
    (swap! o update :dy * factor)))