(ns protektor.core-test
  (:use midje.sweet
        protektor.core))

(comment)
(facts "Traditional exception handling"
       (let [basic-handler (fn [x]
                             (handler-case []
                                           (do
                                             (throw (RuntimeException. (str x)))
                                             (fact "Should never get here" => false))
                                           (RuntimeException [ex]
                                                             :handled)))]
         (fact "Simplistic Handling" (basic-handler ..anything..) => :handled))
       (let [more-elaborate-handler (fn [x]
                                      (handler-case []
                                                    (do (throw x)
                                                        (fact "Should never get here"
                                                              => nil))
                                                    (ArithmeticException [_]
                                                                         :math)
                                                    (IndexOutOfBoundsException [_]
                                                                               :index)
                                                    (NullPointerException [_]
                                                                          :null)
                                                    (RuntimeException [_]
                                                                      :runtime)
                                                    (Exception [_]
                                                               :exception)
                                                    (Error [_]
                                                           :error)
                                                    (Throwable [_]
                                                               :throwable)))]
         (fact "Arithmetic Exception"
               (more-elaborate-handler (ArithmeticException.))
               => :math)
         (fact "Index Out of Bounds"
               (more-elaborate-handler (IndexOutOfBoundsException.))
               => :index)
         (fact "No Such Element"
               (more-elaborate-handler (NullPointerException.))
               => :null)
         (fact "Other Runtime"
               (more-elaborate-handler (RuntimeException.))
               => :runtime)
         (fact "Other Exception"
               (more-elaborate-handler (ClassNotFoundException.))
               => :exception)
         (fact "General Error"
               (more-elaborate-handler (Error.))
               => :error)
         (fact "Root Throwable"
               (more-elaborate-handler (Throwable.))
               => :throwable)))

(comment (facts "Programmatic Restarts"
                (let [test (fn [f]
                             (restart-case []
                                           (throw (RuntimeException. "Failed"))
                                           (..handler1.. (f ..one..))
                                           (..handler2.. (f ..two..))
                                           (..handler3.. (f ..three..))))]
                  (handler-bind [] [RuntimeException (fn [_]
                                                       (invoke-restart ..handler1..))]
                                (fact "#1" (test identity)
                                      => ..one..))
                  (handler-bind [] [RuntimeException (fn [_]
                                                       (invoke-restart ..handler2..))]
                                (fact "#2" (test identity)
                                      => ..two..))
                  (handler-bind [] [RuntimeException (fn [_]
                                                       (invoke-restart ..handler3..))]
                                (fact "#3" (test identity)
                                      => ..three..)))))

;; There isn't any way to automatically test entering a debugger.
;; But what about bindings?

(facts "Building Blocks"
       (fact "Set up locals" (build-lexical-dictionary [ ..k1.. ..v1..
                                                        ..k2.. ..v2..
                                                        ..k3.. ..v3..])
             => {..k1.. ..v1.. ..k2.. ..v2.. ..k3.. ..v3..}))

(comment (facts "Bindings"
                (let [--test-- (fn [f]
                                 (restart-case [a 1]
                                               (throw (RuntimeException. "Error"))
                                               (..handler..
                                                (f locals))))]
                  (handler-bind
                   [RuntimeException [a 2 b 3] #(invoke-restart ..handler..)]
                   (fact (--test-- identity) => {'a 1 'b 3}))
                  (fact (handler-bind 
                         [RuntimeException [b 2 c 3]
                          #(invoke-restart ..handler..)]
                         (--test-- identity))
                        => {'a 1 'b 2 'c 3}))
                (let [--test-- (fn []
                                 (restart-case [x 1 y 2 z 3]
                                               (do
                                                 (fact x => 1)
                                                 (throw (RuntimeException.))
                                                 (..handler1..
                                                  (fact a => 1))
                                                 (..handler2..
                                                  (fact a => 2))
                                                 (..handler3..
                                                  (fact a => 3)))))]
                  (handler-bind
                   [RuntimeException [a 1] #(invoke-restart ..handler1..)])
                  (handler-bind
                   [RuntimeException [a 2] #(invoke-restart ..handler2..)])
                  (handler-bind
                   [RuntimeException [a 3] #(invoke-restart ..handler3..)]))))
