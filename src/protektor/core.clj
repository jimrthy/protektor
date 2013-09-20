(ns protektor.core
  (:require [clojure.tools.trace :refer :all])
  (:gen-class))

;; Map of thread-local bindings where everything interesting is
;; stored. It's really a stack of maps.
;; I've brainstormed about other interesting keys, but the
;; only ones I'm really using so far are
;; :exception (for the class of the exception that is associated with
;; a given restart) and
;; :symbol (to actually identify the restart in question)
(def ^:dynamic *restarts* [])
;; N.B. To make this work the way I want, there needs to be a debug
;; restart bound to Throwable, at the very top of the hierarchy. It
;; will let you explore lexical bindings, values, the call stack
;; (that may be too ambitious) and then select a 'real' restart.


;;; handler-case for "traditional" exception handling.
;;; Note that this limits me to signalling exceptions.
;;; Not sure whether that qualifies as an actual "limitation"
;;; or a feature.

(defn build-lexical-dictionary 
  "This version sort-of works. Except that the names of the supplied
variables will be straight symbols, so this pretty much has to be a macro...
doesn't it?"
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
    `(let ~local-bindings#
       ;; This approach would be significantly less flexible
       ;; than Common Lisp's. That calls an arbitrary
       ;; function that manually invokes a restart. This
       ;; approach is really just about returning the assigned
       ;; restart.
       ;; Both approaches have their pros and cons. Which one
       ;; truly makes more sense?
       ;; This one is less flexible, but involves less code.
       ;; It seems like it would be simple enough to build this
       ;; from that, but not vice-versa.
       (binding [*restarts* ~@(conj *restarts*
                                    (map (fn [[exception-class restart-fn]]
                                           {:name (str restart-symbol)
                                            :handles exception-class
                                            :locals local-dictionary
                                            :description "???"
                                            :action restart-fn
                                            :id (str (gensym))})
                                         (partition 2 associations)))]
         ~@body))))

(defmacro extract-handler
  "Really just destructure an exception-handling data structure. It exists to
simplify destructuring a set of multiple handler clauses into the type
of structure that handler-bind expects."
  [[exception-class
    [exception-instance]
    body]]
  `[,exception-class
    (fn [,exception-instance]
      ,@body)])

;;; Emacs is screwing up formatting with a docstring. I wonder what's busted.

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

So, it'd look something like:
(handler-case
 [local1 (val1)
  local2 (val2)
  local3 (val3)]
 (do
   (do-stuff)
   (do-more-stuff))
 ;; This next syntax feels really wrong.
 [[some-exception [se]
   (recover se)]
  [other-exception [oe]
   (recover se)]])"
  ;; This approach seems to be totally wrong.
  ;; It's fighting against the basic way that java exception handling
  ;; works.
  ;; I'm probably misunderstanding something very fundamental.
  ;; My first guess is that this really shouldn't be built on top of
  ;; handler-bind.
  `(handler-bind ,bindings
                 ~@(map extract-handler handlers)
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

(defn pick-restart 
  "Which restart is currently associated with this exception?"
  [sym restarts]
  (when-let [possibilities (filter (fn [h]
                                     (= (:name h) sym))
                                   restarts)]
    (first possibilities)))

(defn invoke-restart [name]
  (if-let [restart (pick-restart name *restarts*)]
    (throw (Throwable. (:id restart)))
    (throw (RuntimeException. (str "Psych! No restart for '" name
                                   "'\nWhat should hapen now?")))))

(defmacro restart-case [locals body & restarts]
  "Set up exception handling to restart if any are available."
  (let [local-bindings locals
        stack-frame {:name (str (gensym))
                     :handles nil
                     :locals (build-lexical-dictionary local-bindings)
                     :description "???"
                     :action (fn [_] (throw))
                     :id (str (gensym))}]
    `(do
       (println "Binding")
       (binding [*restarts* ~(conj *restarts* stack-frame)]
         (println "Trying")
         (try
           ~body
           (catch Throwable ex#
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
