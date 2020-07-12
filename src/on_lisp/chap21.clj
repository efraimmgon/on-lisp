(ns on-lisp.chap20
  (:require
    clojure.set
    [on-lisp.utils :refer [mac]]))

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
(defn make-proc [{:keys [pri state wait] :as ks}]
  {:pre [(-> ks keys set
             (clojure.set/difference #{:pri :state :wait})
             empty?)]}
  ks)

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

(defmacro fork [expr pri]
  `(let [expr# '~expr]
     (swap! *procs* conj
            (make-proc
              {:state (fn [~(gensym)]
                        ~expr
                        (pick-process))
               :pri   ~pri}))
     expr#))
                  
(comment
  "Allows us to create a group of processes and run them together.")      
(defmacro program [name args & body]
  `(=defn ~name ~args
     (reset! *procs* nil)
     (try
       ~@body
       (catch clojure.lang.ExceptionInfo e#
         (when (= ::halt (-> e# ex-data :cause))
           (loop []
             (pick-process)))))))


(comment
  "pick-process"
  "Selects and runs the highest priority process which is able to restart.")

(comment
  "most-urgent-process"
  "Selects the most urgent process.")

(comment
  "A suspended process is eligible to run if it has no `wait` function, or
  its `wait` fucntion returns true.")

(comment
  "A `wait` is similar to an `=bind`, and carries the same restriction that
  it must be the last thing to be evaluated. Anything we want to happen
  after the `wait` must be put in its body. Thus, if we want to have a
  process wait several times, the wait expressions must be nested.")


;;; Figure 21.2: Process scheduling.

(defn kill [obj & args]
  (if obj
    (swap! *procs* #()))) 
  

(defn pick-process []
  (let [[p val] (most-urgent-process)]
    (reset! *proc* p)
    (swap! *procs* #(remove #{p} %))
    ((:state p) val)))


(comment
  "Figure 21.3: One process with one wait."
  
  (def open-doors (atom nil))
  
  (=defn pedestrian []
    (wait d (first @open-doors)
      (println "Entering" d)))
  
  (program ped []
    (fork (pedestrian) 1)))

(comment
  
  (=defn foo [x]
    (println "Foo was colled with" x)
    (=values (inc x)))
  
  (fork (foo 2) 25)
  
  (program two-foos [a b]
    (fork (foo a) 99)
    (fork (foo b) 99)))