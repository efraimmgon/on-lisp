(ns on-lisp.chap06)

;;;; Functions as Representation

;;; ---------------------------------------------------------------------------
;;; 6.1 Networks

(def nodes
  "Nodes indexed by name"
  (atom {}))

(defn make-node 
  "The `contents` field will contain either a question or a return value.
  If the node is not a leaf, the `yes` and `no` fields will tell where
  to go depending on the answer to the question; if the node is a leaf,
  we will know it because these fields are empty."
  [{:keys [contents yes no] :as m}]
  m)

(defn defnode
  "Makes a new node (of either type) and stores it in `nodes`."
  [{:keys [name contents yes no]}]
  (let [n (make-node
            {:contents contents
             :yes yes
             :no no})]
    (swap! nodes assoc name n)
    n))


(defnode {:name 'people
          :contents "Is the person a man?"
          :yes 'male
          :no 'female})

(defnode {:name 'male
          :contents "Is he living?"
          :yes 'liveman
          :no 'deadman})

(defnode {:name 'deadman
          :contents "Was he American?"
          :yes 'us
          :no 'them})

(defnode {:name 'us
          :contents "Is he on a coin?"
          :yes 'coin
          :no 'cidence})

(defnode {:name 'coin
          :contents "Is the coin a penny?"
          :yes 'penny
          :no 'coins})

(defnode {:name 'penny
          :contents 'lincoln})

#_
(clojure.pprint/pprint @nodes)

(defn run-node
  "Given a name, look up the corresponding `node`. If it is not a leaf,
  the `contents` are asked as a question, and depending on the answer,
  we continue traversing at one of two possible destinations. If the `node`
  is a leaf, just return its `contents`."
  [name]
  (let [n (get @nodes name)]
    (cond
      (:yes n)
      (do (println (:contents n))
          (case (read)
            yes (run-node (:yes n))
            (run-node (:no n))))
      
      :else (:contents n))))  
#_(run-node 'people)

;;; ---------------------------------------------------------------------------
;;; 6.2 Compiling Networks


;;; A network compiled into closures:

(def nodes
  (atom {}))

(defn defnode
  [{:keys [name contents yes no]}]
  (let [n (if yes
            (fn []
              (println contents)
              (case (read)
                yes ((get @nodes yes))
                ((get @nodes no))))
            (fn [] contents))]
    (swap! nodes assoc name n)
    n))

(defnode {:name 'people
          :contents "Is the person a man?"
          :yes 'male
          :no 'female})

(defnode {:name 'male
          :contents "Is he living?"
          :yes 'liveman
          :no 'deadman})

(defnode {:name 'deadman
          :contents "Was he American?"
          :yes 'us
          :no 'them})

(defnode {:name 'us
          :contents "Is he on a coin?"
          :yes 'coin
          :no 'cidence})

(defnode {:name 'coin
          :contents "Is the coin a penny?"
          :yes 'penny
          :no 'coins})

(defnode {:name 'penny
          :contents 'lincoln})

#_((get @nodes 'people))


;;; Compilation with static references:

(def nodes (atom {}))

(defn defnode [{:keys [name conts yes no] :as args}]
  (swap! nodes assoc name (vals args))
  args)

(defnode {:name 'people
          :contents "Is the person a man?"
          :yes 'male
          :no 'female})

(defnode {:name 'male
          :contents "Is he living?"
          :yes 'liveman
          :no 'deadman})

(defnode {:name 'deadman
          :contents "Was he American?"
          :yes 'us
          :no 'them})

(defnode {:name 'us
          :contents "Is he on a coin?"
          :yes 'coin
          :no 'cidence})

(defnode {:name 'coin
          :contents "Is the coin a penny?"
          :yes 'penny
          :no 'coins})

(defnode {:name 'penny
          :contents 'lincoln})

(defn compile-net [root]
  (let [node (get @nodes root)]
    (when (seq node)
      (let [[_ conts yes no] node]
        (if yes
          (let [yes-fn (compile-net yes)
                no-fn (compile-net no)]
            (fn []
              (println conts)
              (if (= (read) 'yes)
                (yes-fn) 
                (no-fn))))
          (fn [] conts))))))
#_(def n (compile-net 'people))
#_(n)