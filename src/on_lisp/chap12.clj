(ns on-lisp.chap12
  (:require
    [on-lisp.utils :refer 
     [mac]]))

;;; 12 - Generalized Variables

;;; ---------------------------------------------------------------------------
;;; 12.1 - The Concept

; An expression which can serve as the first argument to setf is called
; a generalized variable.

(defonce friends (atom {}))

; Make john frind of mary
#_
(swap! friends assoc-in [:mary :john] true)

; Toggle whether x is the friend of y using only built-in operators:
#_
(swap! friends update-in [:mary :john] not)

(defn friends-of [p q]
  (get-in @friends [p q]))

; It's funny how pg's example of how generalized variables are useful 
; makes no sense with clj, because we can compose in a way CL cannot.

; Other than that clj puts emphasis on functional programming, therefore
; writing macros to mutate data structures just does not happen. And when
; we need to do it, the api around atom is very terse and composable,
; as we should above.