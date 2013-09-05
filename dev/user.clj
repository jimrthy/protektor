(ns user
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer (pprint)]
            [clojure.repl :refer :all]
            [clojure.test :as test]
            [clojure.tools.namespace.repl :refer (refresh refresh-all)]
            [protektor.system :as system]
            [protektor.core :as core])
  (:use midje.sweet))

(comment (throw (RuntimeException. "why isn't this loading?")))

;; The system-wide global that doesn't really exist
(def system nil)

(defn init
  "Constructs the current development system"
  []
  (alter-var-root #'system
                  (constantly (system/system))))

(defn start
  "Starts the current development system"
  []
  (alter-var-root #'system system/start))

(defn stop
  "Shuts down and destroys the current development system"
  []
  (alter-var-root #'system
                  (fn [s]
                    (when s (system/stop s)))))

(defn go
  "Initializes the current development system and starts it running"
  []
  (init)
  (start))

(defn reset
  "The heart of the workflow"
  []
  (stop)
  (refresh :after 'user/go))


(defmacro p-expand
  "Macro-expand body once and pretty-print the outcome"
  [body]
  `(pprint (macroexpand-1 ~body)))
