(ns on-lisp.chap23
  (:require
    [on-lisp.utils :refer [mac]]
    [on-lisp.chap20 :refer [=defn =values =bind cont]]
    [on-lisp.chap22 :refer [choose fail]]))

;;; 23 - Parsing with ATNs

;;; ---------------------------------------------------------------------------
;;; 23.1 Background

(comment
  "ATNs are useful if you observ the following four restrictions:
  
  1. Use them in a semantically limited domain - in a front-end to a 
  particular database, for example.
  
  2. Don't feed them very difficult input. Among other things, don't expect
  them to understand wildly ungrammatical sentences the way people can.
  
  3. Only use them for English, or other languages in which word order
  determines grammatical structure. ATNs would not be useful in parsing
  inflected languages like Latin.
  
  4. Don't expect them to work all the time. Use them in applications where
  it's helpful if they work ninety percent of the time. not those where it's
  critical that they work a hundred percent of the time.")


;;; ---------------------------------------------------------------------------
;;; 23.2 The Formalism

(comment
  "Figure 23.1: A very small ATN."
  
  (defnode s
    (cat noun s2
         (setr subj *)))
  
  (defnode s2
    (cat verb s3
         (setr v *)))
  
  (defnode s3
    (up `(sentence
           (subject ~(getr subj))
           (verb ~(getr v))))))


;;; ---------------------------------------------------------------------------
;;; 23.4 An ATN Compiler

(comment
  "The parameter `pos` will be the current position in the input sentence,
  and `regs` the current reigsters."
  
  "Cat arcs insist that the current word of input belong to acertain 
  grammatical category. Within the body of a cat arc, the symbol `*` will be
  bound to the current word of input."
  
  "Push arcs, defined with `down` require successful calls to sub-networkds.
  They take two destination nodes, the sub-network destination `sub`, and
  the next node in the current network, `next`. "
  
  "The final type of arc is the pop arc, defined with `up`. Pop arcs are
  unusual in that they don't have a destination. ")

(def sent (atom nil))
(def paths (atom nil))

(defn compile-cmds [cmds]
  (if (empty? cmds)
    'regs
    `(~@(first cmds) ~(compile-cmds (rest cmds)))))

(defn types [word]
  (case word
    (do does did) '(aux v)
    (time times) '(n v)
    (fly flies) '(n v)
    (like) '(v prep)
    (liked likes) '(v)
    (a an the) '(det)
    (arrow arrows) '(n)
    (i you he she him her it) '(prn)
    nil))
                                          
(defmacro defnode [name & args]
  `(=defn ~name [~'pos ~'regs] (choose ~@args)))

(defmacro down [sub next & cmds]
  `(=bind (* ~'pos ~'regs) (~sub ~'pos (cons nil ~'regs))
     (~next ~'pos ~(compile-cmds cmds))))

(defmacro cat [cat next & cmds]
  `(if (= (count @sent) ~'pos)
     (fail)
     (let [~'* (nth ~'pos @sent)]
       (if (some (fn [x#] (= x# '~cat)) (types ~'*))
         (~next (inc ~'pos) ~(compile-cmds cmds))
         (fail)))))

(defmacro jump [next & cmds]
  `(~next ~'pos ~(compile-cmds cmds)))

(defmacro up [expr]
  `(let [~'* (nth ~'pos @sent)]
     (=values ~expr ~'pos (rest ~'regs))))

(defmacro getr [k & [regs]]
  (let [regs (or regs 'regs)]
    `(let [~'result (rest (some (fn [x#] 
                                  (and (= '~k (first x#))
                                       x#))
                                (first ~regs)))]
       (if (seq (rest result))
         result
         (first result)))))

(defmacro set-register [k v regs]
  `(cons (cons (cons ~k ~v) (first ~regs))
         (rest ~regs)))

(defmacro setr [k v regs]
  `(set-register '~k (list ~v) ~regs))

(defmacro pushr [k v regs]
  `(set-register '~k
                 (cons ~v (rest (some (fn [x#] 
                                        (and (= '~k (first x#))
                                             x#))
                                      (first ~regs))))
                 ~regs))
