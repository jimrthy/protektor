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

(defn extract-handler
  "This basically does what I want...except that you're limited to a single body form."
  [[exception-class
    [exception-instance]
    body]]
  (list 'catch exception-class exception-instance body))

;; Is there any real point to this being a macro?
(defmacro handler-case [bindings body & handlers]
  (let [local-bindings bindings
        local-name (gensym)
        local-dictionary (build-lexical-dictionary local-bindings)
        stack-frame {:name (str local-name)
                     :handles nil
                     :locals local-dictionary
                     :description "???"
                     :action (fn [_] (throw))
                     :id (str (gensym))}]
    `(binding [*restarts* ~(conj *restarts* stack-frame)]
       (let ~local-bindings
         (try
           ~body
           ~@(map extract-handler handlers))))))

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

;;; Associate an exception class with a restart.
;;; Note that this really needs to set up values that will be visible to
;;; the debugger. What's a good way to handle that?
(defmacro handler-bind [locals [& associations] & body]
  (let [local-bindings locals]
    `(let ~local-bindings
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
                                    (map (fn [[exception-class restart-symbol]]
                                           {:name (str restart-symbol)
                                            :handles exception-class
                                            :locals (build-lexical-dictionary local-bindings)
                                            :description "???"
                                            :action restart-symbol
                                            :id (str (gensym))})
                                         (partition 2 associations)))]
         ~@body))))

(defn -main
  "This is a library...not a program.
Though invoking the unit tests might make sense here.
Might also be interesting to treat it as a REPL with
a built-in debugger."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (println "Hello, World!"))
