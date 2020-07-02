(ns on-lisp.chap19
  (:require
    [on-lisp.utils :refer [mac]]))

;;; 19 - A Query Compiler

;;; ---------------------------------------------------------------------------
;;; 19.1 The Database

;;; Figure 19.1: Basic database functions

(defn make-db []
  {}) 

(def default-db (atom (make-db)))

(defn clear-db 
  ([]
   (clear-db default-db))
  ([db]
   (reset! db {})))
  
(defn db-query
  ([key]
   (db-query key default-db))
  ([key db]
   (get @db key)))

(defn db-push 
  ([key val]
   (db-push key val default-db))
  ([key val db]
   (swap! db update key conj val)))

(defmacro fact [pred & args]
  `(do (db-push '~pred '~args)
       '~args))

#_(fact painter reynolds joshua english)
#_(fact painter canale antonio venetian)
#_(db-query 'painter)