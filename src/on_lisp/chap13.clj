(ns on-lisp.chap13
  (:require
    [on-lisp.utils :refer [mac]]))

;;; 13 - Computation at Compile-Time

;;; ---------------------------------------------------------------------------
;;; 13.1 New Utilities

;;; Figure 13.1: Shifting computation when finding averages.

(defn avg [& args]
  (float (/ (apply + args)
            (count args))))

(defmacro avg [& args]
  `(float (/ (+ ~@args)
             ~(count args))))

#_(avg Math/PI 4 5)

;;; Figure 13.2: Shifting and avoiding computation.

(defn most-of 
  "Returns true if most of its arguments do."
  [& args]
  (let [hits (filter #(and %) args)]
    (> (count hits)
       (/ (count args) 2))))

(defmacro most-of
  "Returns true if most of its arguments do."
  [& args]
  `(> ~(count (filter #(and %) args))
      (/ ~(count args) 2)))

#_(mac (most-of true true true nil))

;;; 13.3: Use of arguments known at compile-time

(defn nthmost
  "Takes a number and a list of numbers, and returns the nth largest
  among them."
  [n coll]
  (nth (sort > coll) n))

; If you wanted to find, say, the third biggest cookie on a plate, you could
; do it by looking at each cookie in turn, always keeping in your hand the
; three biggest found so far. When you have looked at all the cookies, the
; smallest cookie in your hand is the one you are looking for. If `n` is
; a small constant, not proportional to the number of cookies, then this
; technique gets you a given cookie with less effort that it would take
; to sort all of them first.
(defn nthmost [n coll]
  (if (< n 20)
    (when-not (< (count coll) (inc n))
      (loop [acc (->> coll (take (inc n)) (sort >))
             [x & more :as remain] (drop (inc n) coll)]
        (if-not (seq remain)
          (last acc)
          (recur
            (let [newacc
                  (reduce (fn [ret y]
                            (if (> x y)
                              (conj ret x y)
                              (conj ret y)))
                          [] acc)]
              (if (> (count newacc) (inc n))
                (butlast newacc)
                newacc))
            more))))
    (nth (sort > coll) n)))
  
#_
(nthmost 2 [2 6 1 5 3 4])

;;; ---------------------------------------------------------------------------
;;; 13.2 Example: Bezier Curves


;;; Figure 13.5: Macro for generating Bezier curves.

(defn make-array* [[x y]]
  (mapv (fn [i]
          (vec (take y (repeat nil))))
        (range x)))
#_(make-array* [2 3])

(def segs 20)
(def du (/ 1.0 segs))
(def pts (atom (make-array* [(inc segs) 2])))

; A Bezier curve is defined in terms of four points - two endpoints and two
; control points. When we are working in two dimensions, these points 
; define parametric equations for the x and y coordinates of points on 
; the curve. If the two endpoints are (x0, y0) and (x3, y3) and the two
; control points are (x1, y1) and (x2, y2), then the equations defining the
; points on the curve are:
; x = (x3 - 3x2 + 3x1 - x0)u3 + (3x2 - 6x1 + 3x0)u2 + (3x1 - 3x0)u + x0
; y = (y3 - 3y2 + 3y1 - y0)u3 + (3y2 - 6y1 + 3y0)u2 + (3y1 - 3y0)u + y0
; If we evaluate these equations for n values of u between 0 and 1, we get n
; points on the curve.
; If we evaluate thse equations for n values of u between 0 and 1, we get n
; points on the curve. For example, if we want to draw the curve as 20 
; segments, then we would evaluate the equations for u = 0.05, 0.1, ..., 0.95.
; There is no need to evaluate them for u of 0 or 1, because if u = 0 they
; will yield the first endpoint (x0, y0) and if u = 1 they will yield the 
; second endpoint (x3, y3).
(defmacro genbez [x0 y0 x1 y1 x2 y2 x3 y3]
  `(let [gx0# ~x0 
         gy0# ~y0
         gx1# ~x1
         gy1# ~y1
         gx3# ~x3
         gy3# ~y3
         cx (* (- gx1# gx0#) 3)
         cy (* (- gy1# gy0#) 3)
         px (* (- ~x2 gx1#) 3)
         py (* (- ~y2 gy1#) 3)
         bx (- px cx)
         ax (- gx3# px gx0#)
         ay (- gy3# py gy0#)]
     (swap! pts assoc-in [0 0] gx0#)
     (swap! pts assoc-in [0 1] gy0#)
     ~@(mapcat (fn [n]
                 (let [u (* n du)
                       u2 (* u u)
                       u3 (* u u u)]
                   (vector
                     `(swap! pts assoc-in [~n 0]
                             (+ (* ax ~u3)
                                (* bx ~u2)
                                (* cx ~u)
                                gx0#))
                     `(swap! pts assoc-in [~n 1]
                             (+ (* ay ~u3)
                                (* by ~u2)
                                (* cy ~u)
                                gy0#)))))
            (range 1 segs))
     (swap! pts assoc-in [segs 0] gx3#)
     (swap! pts assoc-in [segs 1] gy3#)))
       