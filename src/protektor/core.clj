(ns protektor.core
  (:require [clojure.tools.trace :refer :all])
  (:use [slingshot.slingshot :only [throw+ try+]])
  (:gen-class))

;; All these dynamic "globals" are a definite code smell.
;; FIXME: At the very least, they should be private.
;; I don't like them. At the same time...they're exactly
;; what I need. Or so it seems.

;;; Map of thread-local bindings where the restart functions are
;;; stored. It's really a stack of maps.
(def ^:dynamic *restarts* [])
;; N.B. To make this work the way I want, there needs to be a debug
;; restart bound to Throwable, at the very top of the hierarchy. It
;; will let you explore lexical bindings, values, the call stack
;; (that may be too ambitious) and then select a 'real' restart.
;; Most likely, I also want to set up the JVM's UncaughtExceptionHandler
;; with something that does pretty much exactly the same thing.

;;; Which restart should be invoked for any given condition being
;;; signalled? 
(def ^:dynamic *restart-bindings* {})

(def ^:dynamic *call-stack* [])

(defn build-lexical-dictionary 
  "Convert local variables (from something like a let form) into
a dictionary, to make it more reasonable to examine the bindings later."
  [ls]
  ;; This name is wrong, and the idea is only half-baked at best.
  ;; Really want to track the bindings that are active at different
  ;; levels of the call stack.
  ;; That information probably won't be available for a general
  ;; case. So go with what I can (which is based on declaring
  ;; bindings around signal handling).
  (let [pairs (partition 2 ls)
        keys (map first pairs)
        vals (map second pairs)
        result (zipmap keys vals)]
    result))

(defmacro handler-bind [locals [& associations] & body]
  "Associate exception classes with a restarts.
Parameters:
locals: establish local bindings, using let
associations: a seq of handler pairs. Each pair consists of
1) The type of condition to handle
2) A 1-arg function that handles the condition.

Major difference between this and handler-case (see below):
handler-case unwinds the stack. handler-bind does not.

Note that this really needs to set up values that will be visible to
the debugger. What's a good way to handle that?"
  ;; This pair of nested lets seems like a horrible approach.
  (let [local-bindings# locals
        local-dictionary# (build-lexical-dictionary local-bindings#)]
    `(let local-bindings#
       (binding [*restart-bindings* ~@(conj *restart-bindings*
                                    (map (fn [[exception-class restart-fn]]
                                           {:handles exception-class
                                            :action restart-fn
                                            :id (str (gensym))})
                                         (partition 2 associations)))
                 *call-stack* (conj *call-stack* local-dictionary#)]
         ~@body))))

(defmacro handler-case [bindings body & handlers]
  "'classic' try/catch sort-of handler. Except that it lets you set up bindings around your handlers.
For the benefit of a [currently] hypothetical debugger.

Quoting from _Practical Common Lisp_:
HANDLER-CASE is the nearest analog in Common Lisp to Java- or Python-style exception handling.

Where you might write this in Java:

  try {
    doStuff();
    doMoreStuff();
  } catch (SomeException se) {
    recover(se);
  }

or this in Python:

  try:
    doStuff()
    doMoreStuff()
  except SomeException as se:
    recover(se)

In Common Lisp you'd write this:

 (handler-case
  (progn
    (do-stuff)
    (do-more-stuff))
  (some-exception (se) (recover se)))

This is my attempt to make this look at least somewhat like idiomatic clojure.

I think it should look something like:
 (handler-case
 [local1 (val1)
  local2 (val2)
  local3 (val3)]
 (do
   (do-stuff)
   (do-more-stuff))
 [[some-exception [se]
   (recover se)]
  [other-exception [oe]
   (recover se)]])"
  `(handler-bind ,bindings
                 [~@(map (fn [[ex-class ex-instance ex-handler]]
                           `[,ex-class
                             (fn [,ex-instance]
                               ,@ex-handler)]))]
                 ~body))

;;; More interesting pieces

(defn active-restart
  "What is the symbol identifying the restarts associated with a
given Throwable class?
The interesting part is that different levels could bind different
restarts to different exceptions"
  [thrown]
  (println "Trying to find a restart for: " thrown "\nin " *restarts*)
  ;; Don't actually need to assert here. Except that this really
  ;; shouldn't be called unless at least 1 restart is set up.
  ;; For that matter, the debugging restart should always be
  ;; configured. But that's getting ahead of myself.
  (assert (seq *restarts*))
  (throw (RuntimeException. "Why did I comment this all out?"))
  (comment (->> *restarts*
                (filter (fn [restart]
                          (println "Checking: " restart)
                          (when-let [ex (:handles restart)]
                            (instance? ex thrown))))
                first
                :symbol))
  (let [filtered (filter (fn [r]
                           (println "Checking: " r)
                           (when-let [ex (:handles r)]
                             (instance? ex thrown)))
                         *restarts*)]
    (println "Potentials: " filtered)
    (let [dict (first filtered)]
      (println "Chosen: " dict)
      (let [s (:symbol dict)]
        (println "Symbol: " s)
        s))))

(defn pick-restarts
  "Which restart is currently associated with this exception?"
  [sym restarts]
  (filter (fn [h]
            (= (:name h) sym))
          restarts))

(defn invoke-restart [name]
  (if-let [restart (pick-restarts name *restarts*)]
    ;; Really shouldn't be throwing anything
    (throw+ {:id restart})
    (throw (RuntimeException. (str "Psych! No restart for '" name
                                   "'\nWhat should happen now?")))))

(defn signal [condition]
  ;; Something at least vaguely out-of-the-ordinary happened
  ;; which involves some sort of out-of-band communication up
  ;; the stack layer.
  (loop [restarts (pick-restarts condition *restarts*)]
    (when-let [r (first restarts)]
      (let [action (:action r)]
        ;; This is where things get sticky.
        ;; In common lisp, this really works outside the
        ;; runtime. Any restart that returns at all
        ;; didn't handle the condition.
        ;; Which means that this should be a special form.
        ;; Since that luxury really isn't available,
        ;; fudge things.
        ;; Restarts are required to wrap their return value
        ;; into a vector. Its first member is whether the
        ;; restart was handled.
        ;; If it was, the second is the "real" return value
        ;; from the signal.
        (let [result-pair (action r)
              handled? (first result-pair)]
          (if handled? 
            result-pair ; N.B. Returns the handled? indicator!!
            (recur (rest restarts))))))))

(defn warn [w]
  (let [[handled? result] (signal w)]
    (when-not handled?
      ;; If warnings aren't muffled, send w to STDERR.
      ;; FIXME: Start here
      (throw (RuntimeException. "Get this written!"))
      )))



;;; Q: What should restart-case expand into?

(defmacro restart-case [locals body & restarts]
  "Set up exception handling to restart if any are available.
The initial (let) looks awfully familiar...is it even vaguely
reusable? Even if it means a macro-in-a-macro?
Run body inside restarts"
  (let [local-bindings locals
        locals-dictionary (build-lexical-dictionary local-bindings)
        stack-frame {:name (str (gensym))
                     :action (fn [_] (throw))
                     :id (str (gensym))}]
    `(do
       (println "Binding")
       (binding [*restarts* ~(conj *restarts* stack-frame)
                 *call-stack* ~(conj *call-stack* (locals-dictionary))]
         (println "Trying")
         (try+
          ;; This doesn't work. The call stack still unwinds up
          ;; to here.
          ;; So this approach would be broken, even if the macro
          ;; worked.
           ~body
           (catch Object ex#
             (println "Caught: " ex#)
             (if-let [restart-symbol# (active-restart ex#)]
               (do
                 (println "Restart Symbol: " restart-symbol#)
                 (if-let [restart# (pick-restart restart-symbol# ~@restarts)]
                   (do
                     (println "Restart: " restart# "Message: " (.getMessage ex#))
                     (if (= (:id restart#) (.getMessage ex#))
                       ;; N.B. I want to be careful not to unwind the stack
                       ;; unless that option is selected.
                       ;; If there's no associated handler, we should wind
                       ;; up in the debugger at the point of the exception.
                       ;; Building this to depend on the Java exception
                       ;; and class inheritance systems seems like an
                       ;; implementation detail that
                       ;; would be better to avoid if possible.
                       ;; It means that, honestly, I'm planning for a breaking
                       ;; change.
                       ;; Oh well. I have to start somewhere.
                       ((:action restart#))
                       (throw)))
                   (throw)))
               (throw))))))))

(defn -main
  "This is a library...not a program.
Though invoking the unit tests might make sense here.
Might also be interesting to treat it as a REPL with
a built-in debugger."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (println "Currently, running this as a stand-alone just doesn't make any sense."))
