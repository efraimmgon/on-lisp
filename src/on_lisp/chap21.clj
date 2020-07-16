(ns on-lisp.chap21
  (:require
    clojure.set
    [on-lisp.utils :refer [mac]]
    [on-lisp.chap20 :refer [=defn =values cont]]))

;;; 21 - Multiple Processes

;;; ---------------------------------------------------------------------------
;;; 21.1 The Process Abstraction

(comment
  "The aim of this chapter is to make a language which supports multiple
  processes. Our strategy will be to turn Lisp into sucha language, by 
  adding a few new operators. The basic elements of our language will be
  as follows:
  
  *Functions* will be defined with the `=defn` or `=lambda` macros from the
  previous chapter.
  
  *Processes* will be instantiated from function calls. There is no limit on
  the number of active processes, or the number of processes instantiated
  from any one function. Each process will have a priority, initially given
  as an argument when it is created.
  
  *Wait expressions* may occur within functions. A wait expression will take
  a variable, a test expression, and a body of code. If a process encounters
  a wait. the process will be suspended at that point until the test 
  expression returns true. Once the process restarts, the body of code will
  be evaluated, with the variable bound to the value of the test expression.
  Test expressions should not ordinarily have side-effects, because there are 
  no guarantees about when, or how often, they will be evaluated.
  
  *Scheduling* will be done by priority. Of all the processes able to restart,
  the system will run the one with the highest priority.
  
  *The default process* will run if no other process can. It is a read-eval-
  print loop.
  
  *Creation and deletion* of most objects will be possible on the fly. From
  running processes it will be possible to define new functions, and to
  instantiate and kill processes.")


;;; ---------------------------------------------------------------------------
;;; 21.2 Implementation

;;; Figure 21.1: Process structure and instantiation.

(comment
  "Processes, or `procs`, have the following structure:
  `pri` is the priority of the process, which should be a positive number.
  `state` is a continuation representing the state of a suspended process. A
  process is restarted by calling its `state`.
  `wait` is usually a function which must return true in order for the 
  process to be restarted, but initially the wait of anewly created process
  is nil. A process with a null `wait` can always be restarted.")

(defrecord Proc [pri state wait])

(defn make-proc [{:keys [pri state wait] :as ks}]
  {:pre [(-> ks keys set
             (clojure.set/difference #{:pri :state :wait})
             empty?)]}
  (map->Proc ks))

(def ^:dynamic *procs* 
  "The list of currently suspended processes."
  (atom nil))

(def ^:dynamic *proc*
  "The process now running."
  (atom nil))

(declare pick-process)

(comment
  "Runs only when no other process can. It simulates the Lisp toplevel.
  Within this loop, the user can halt the program, or type expressions which
  enable suspended processes to restart. Notice that the default process
  calls `eval` explicitly.")
(def default-proc
  "The default process."
  (make-proc {:state (fn [x]
                       (newline)
                       (print ">> ")
                       (prn (eval (read)))
                       (pick-process))}))

(defmacro fork 
  "Instantiates a process from a function call. Functions are defined as usual
  with `=defn`. Takes an expression and a priority (a positive int)."
  [expr pri]
  `(let [expr# '~expr]
     (swap! *procs* conj
            (make-proc
              {:state (fn [g#]
                        ~expr
                        (pick-process))
               :pri   ~pri}))
     expr#))
                  
(comment
  "Allows us to create a group of processes and run them together. The program
  can be ended with `halt`.")      
(defmacro program [name args & body]
  `(=defn ~name ~args
     (reset! *procs* nil)
     (try
       ~@body
       (loop [] (pick-process))
       (catch clojure.lang.ExceptionInfo e#
         (when (= ::halt (-> e# ex-data :cause))
           (-> e# ex-data :val))))))
  

;;; Figure 21.2: Process scheduling.

(defn most-urgent-process 
  "Selects the (elegible) process with highest priority. A process is elegible
  to run if it has no `wait` function, or its `wait` function returns true.
  There will always be some winning process, beause the default process always
  wants to run."
  []
  (let [[proc max val]
        (reduce
          (fn [[proc1 max val1 :as acc] p]
            (let [pri (:pri p)]
              (if (> pri max)
                (let [val (or (not (:wait p))
                              ((:wait p)))]
                  (if val
                    [p pri val]
                    acc))
                acc)))
          [default-proc -1 true] @*procs*)]
    [proc val]))

(defn pick-process 
  "Selects and runs the highest priority process which is able to restart."
  []
  (let [[p val] (most-urgent-process)]
    (reset! *proc* p)
    (swap! *procs* #(remove #{p} %))
    ((:state p) val)))

(comment
  "Individual processes can be killeed by calling `kill`. If given no 
  arguments, this operator kills the current process. In this case, `kill`
  is like a wait expression which neglects to store the current process. If
  `kill` is given arguments, they become the arguments to a delete on the
  list of processes.")
(defn kill 
  ([] 
   (kill nil))
  ([f]
   (if f
     (swap! *procs* #(remove f %))
     (pick-process))))

(comment
  "Stops the whole program, by throwing control back to the tag established by
  the expansion of program. It takes an optional argument, which will be 
  returned as the value of the program.")
(defn halt 
  ([] 
   (halt nil))
  ([val]
   (throw (ex-info "Halt" {:cause ::halt :val val}))))

(defn setpri [n]
  (swap! *proc* assoc :pri n))

(comment
  "This function stores the current process, and then calls `pick-process` to
  start some process (perhaps the same one) running again. It will be given
  two arguments: a test function and a continuation. The former will be stored
  as the `proc-wait` of the process being suspended, and called later to
  detrmine if it can be restarted. The latter will become the `proc-state`, and
  calling it will restart the suspended process.")
(defn arbitrator [test cont]
  (swap! *proc* assoc :state cont)
  (swap! *proc* assoc :wait test)
  (swap! *procs* conj @*proc*)
  (pick-process))

(comment
  "There is another, simpler type of wait expression: `yield`, whose only
  purpose is to give other higher-priority processes a chance to run. A
  process might want to yield after executing a setpri expression, which 
  resets the priority of the current process. As with a `wait`, any code to
  be executed after a `yield` must be put within its body.")
(defmacro yield [& body]
  `(arbitrator nil (fn [x#] ~@body)))

(comment
  "A `wait` is similar to an `=bind`, and carries the same restriction that
  it must be the last thing to be evaluated. Anything we want to happen
  after the `wait` must be put in its body. Thus, if we want to have a
  process wait several times, the wait expressions must be nested.")
(defmacro wait [param test & body]
  `(arbitrator (fn [] ~test)
               (fn [~param] ~@body)))


(comment
  "Figure 21.3: One process with one wait.")
  
(def open-doors (atom nil))

(=defn pedestrian []
  (wait d (first @open-doors)
    (println "Entering" d)
    (flush)))

(program ped []
  (fork (pedestrian) 1))

(comment
  (ped)

  (swap! open-doors conj 'door2))
  
(comment
  
  (=defn foo [x]
    (println "Foo was colled with" x)
    (=values (inc x)))
  
  (fork (foo 2) 25)
  
  (program two-foos [a b]
    (fork (foo a) 99)
    (fork (foo b) 99)))


;;; Figure 21.4: Synchonization with a blackboard.

(def bboard (atom nil))

(defn claim [& f] (swap! bboard conj f))

(defn unclaim [& f] (swap! bboard #(remove #{f} %)))

(defn check [& f] (some #(and (= % f) f) @bboard))
                        
(=defn visitor [door]
  (println "Approach" door)
  (claim 'knock door)
  (wait g (check 'open door)
    (println "Enter" door)
    (unclaim 'knock door)
    (claim 'inside door)))

(=defn host [door]
  (wait k (check 'knock door)
    (println "Open" door)
    (claim 'open door)
    (wait g (check 'inside door)
      (println "Close" door)
      (unclaim 'open door))))

(program ballet []
  (fork (visitor 'door1) 1)
  (fork (host 'door1) 1)
  (fork (visitor 'door2) 1)
  (fork (host 'door2) 1))


;;; Figure 21.5: Effect of changing priorities.

(defn ttake [c] (println "Liberating" c))
(defn fortify [c] (println "Rebuilding" c))
(defn loot [c] (println "Nationalizing" c))
(defn ransom [c] (println "Refinancing" c))

(=defn capture [city]
  (ttake city)
  (setpri 1)
  (yield 
    (fortify city)))

(=defn plunder [city]
  (loot city)
  (ransom city))

(program barbarians []
  (fork (capture 'rome) 100)
  (fork (plunder 'rome) 98))


;;; ---------------------------------------------------------------------------
;;; 21.3 The Less-than-Rapid Prototype


