(ns protektor.example
  (:genclass))

;;;; I don't have a solid idea about where this is going...
;;;; mainly just brainstorming ideas during my commute.

;;; Re-bind this as a thread-local when entering a new
;;; handler stack
(def ^:dynamic *handlers* (list {:name "Exit"
                                 :handles Object
                                 :locals {}
                                 :description "Give up and stop the JVM."
                                 :action (fn [_] (system/exit 0))
                                 :id (gensym)}))

(defn condition-picker
  "Use something like this for whatever UI might be appropriate.
In particular, actual interaction might make something resembling 
a debugger feasible.

As it is, I'm going back to my DOS menuing roots."
  [potential-handlers ex]
  (println ex)
  (let [handlers (map-indexed (fn [h i]
                                (format "%d. %s -- %s" i 
                                        (:name h) (:description h)))
                              potential-handlers)]
    (doseq [s handlers] (println s))
    ;; This is a debugging tool. Go ahead and live dangerously.
    (let [decision (read-string)
          index (dec decision)
          ;; Yeah, yeah. This is also super dangerous.
          ;; Leave me alone...I'm hacking.
          selection (nth handlers index)]
      selection)))

;;; Really want a way to set handlers to trigger automatically.
;;; Check each of the automated handlers in turn. If any
;;; returns t, consider the error handled and proceed.
;;; If they all return nil, call this.
;;; That isn't right, but it's a little closer.
(def ^:dynamic *picker* condition-picker)

(defn error 
  "Something unexpected happened. Give the user a chance to fix it and retry."
  [ex]
  (let [potential-handlers (filter #(instance? ex %))
        handler (*picker* potential-handlers ex)]
    ;; FIXME: Actually, I want to unwind the callstack.
    ;; That turns a little ugly...how do I specify which
    ;; handler I'm throwing to?
    ;; It should probably have something to do with the
    ;; id...can I add that to the exception instance?
    ((:action handler) ex)))

(defn warn
  "Something unusual happened. It probably isn't worth risking program
exit, but it's worth noting. Any handlers could, of course, trigger
an error"
  [ex]
  (throw (RuntimeException. "FIXME: Implement this")))

(defn signal
  "I have mixed feelings about this. Use Java's exception system to
transfer control somewhere arbitrarily up the call stack.
Definitely need to consider whether I want to do this or not."
  [ex]
  (throw (RuntimeException. "FIXME: Implement this")))
