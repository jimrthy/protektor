(ns protektor.core
  (:gen-class))

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
  (comment `(catch ~exception-class ~exception-instance ~body))
  (list 'catch exception-class exception-instance body))

(defmacro handler-case [bindings body & handlers]
  (let [locals (gensym)]
    `(let ~bindings
       (let [~locals ~(build-lexical-dictionary bindings)]
         (try
           ~body
           ;; Can't map to a macro
           (comment)
           ~@(map extract-handler handlers))))))

;;; More interesting pieces

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

(defn active-restart
  "What is the symbol identifying the restarts associated with a
given Throwable class?
The interesting part is that different levels could bind different
restarts to different exceptions"
  [thrown]
  (->> *restarts*
       (filter (fn [restart]
                 (instance? (:exception restart) thrown)))
       first
       :symbol))

(defn pick-restart 
  "Which restart is currently associated with this exception?"
  [sym restarts]
  (when-let [possibilities (filter (fn [h]
                                     (= (first h) sym))
                                   restarts)]
    (first possibilities)))

(defmacro restart-case [locals body & restarts]
  "Set up exception handling to restart if any are available."
  `(let ~locals
     (let [locals ~(build-lexical-dictionary locals)]
       (try
         ~body
         (catch Throwable ex
           (if-let [restart-symbol (active-restart (ex))] 
            (if-let [restart (pick-restart restart-symbol restarts)]
               ((second restart))
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
               (throw))
             (throw)))))))

;;; Associate an exception class with a restart.
;;; Note that this really needs to set up values that will be visible to
;;; the debugger. What's a good way to handle that?
(defmacro handler-bind [locals [& associations] & body]
  `(let locals
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
     (binding [*restarts* ~(conj 
                            (doall (map (fn [[exception-class restart-symbol]]
                                          {:exception exception-class
                                           :symbol restart-symbol
                                           :locals locals})
                                        associations))
                            *restarts*)]
       ~body)))

(defn -main
  "This is a library...not a program.
Though invoking the unit tests might make sense here.
Might also be interesting to treat it as a REPL with
a built-in debugger."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (println "Hello, World!"))
