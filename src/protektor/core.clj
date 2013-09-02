(ns protektor.core
  (:gen-class))

;;; handler-case for "traditional" exception handling.
;;; Note that this limits me to signalling exceptions.
;;; Not sure whether that qualifies as an actual "limitation"
;;; or a feature.

(defn build-lexical-dictionary [ls]
  (throw (RuntimeException. "Do something with this")))

(defn extract-handler [[exception-class
                        [exception-instance]
                        & body]]
  (list 'catch exception-class body))

(defmacro handler-case [locals body & handlers]
  `(let ~locals
     (let [locals ~(build-lexical-dictionary locals)]
       (try
         ~body
         (map extract-handler handlers)))))

;;; More interesting pieces

(defn active-restart
  "What restarts are associated with a given Throwable class?"
  [thrown]
  (throw (RuntimeException. "Not Implemented Yet")))

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
               (throw))
             (throw)))))))

;;; Associate an exception class with a restart.
;;; Note that this really needs to set up values that will be visible to
;;; the debugger. What's a good way to handle that?
(defmacro handler-bind [locals [& associations] & body]
  (throw (RuntimeException. "Get this written!")))

(defn -main
  "This is a library...not a program.
Though invoking the unit tests might make sense here.
Might also be interesting to treat it as a REPL with
a built-in debugger."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (println "Hello, World!"))
