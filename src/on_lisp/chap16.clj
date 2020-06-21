(ns on-lisp.chap16
  (:require
    [on-lisp.utils :refer [mac]]))

;;; 16 - Macro-Defining Macros

;;; ---------------------------------------------------------------------------
;;; 16.1 Abbreviations

(defmacro abbrev [short long]
  `(defmacro ~short [& args#]
     `(~'~long ~@args#)))

(defmacro abbrevs [& names]
  `(do
     ~@(map (fn [pair]
              `(abbrev ~@pair))
            (partition 2 names))))

;; Sample expansion:
#_
(defmacro bindn [& args]
  `(binding ~@args))

;; Pull `binding` from within the backquote:
#_
(defmacro bindn [& args]
  (let [name 'binding]
    `(~name ~@args)))

;; Now turn it into a template: affix a backquote, and replace the expressions
;; which will vary, with variables:
#_
`(defmacro ~short [& ~'args]
   (let [name '~long]
     `(~name ~@~'args)))

;; Final step is to substitute '~long for name within the inner backquote:
#_
`(defmacro ~short [& ~'args]
   `(~'~long ~@~'args))

#_
(mac
  (abbrev summing +))

;;; ---------------------------------------------------------------------------
;;; 16.3 Anaphoric Macros

(defn anaphex1 [args call]
  (if (seq args)
    (let [sym (gensym)]
      `(let [~sym ~(first args)
             it ~sym]
         ~(anaphex1 (rest args)
                    (concat call (list sym)))))
    call))
         
(defn anaphex2 [op args]
  `(let [it ~(first args)]
     (~op ~'it ~@(rest args))))

(defmacro defanaph [name & [{:keys [rule calls]}]]
  (let [rule (or rule :all)
        opname (or calls (-> name str first str symbol))
        body (case rule
               :all `(anaphex1 ~'args '(~opname))
               :first `(anaphex2 '~opname ~'args))]
    `(defmacro ~name [& ~'args]
       ~body)))

(mac
  (defanaph alist))
(mac 
  (defanaph aif {:rule :first}))