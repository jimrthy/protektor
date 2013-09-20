(ns protektor.log-test
  (require [protektor.core :as p]
           [clojure.edn :as edn])
  (use midge.sweet))

;;; Inspired by _Practical Common Lisp_
(defn find-all-logs []
  "Pretend to pull back a bunch of logs from somewhere.
Realistically, these should be lines of text. Although it's almost
definitely to save logs to a database instead of just a txt file."
  [[#inst "2013-09-20T12:37:00.000-00:00" [:normal "Entry One"]]
   [#inst "2013-09-20T12:38:00.000-00:00" [:error "Fail"]]
   [#inst "2013-09-20T12:38:30.000-00:00" [:warn "Something special should probably happen with this"]]
   [#uuid "17c28986-2009-40f9-850f-ad5d389788c7" [:normal "This shouldn't work either"]]
   [#inst "2013-09-20T12:54:00.000-00:00" [:normal "This shouldn't work"] "extra: causes error"]])

(defn valid-entry? [entry]
  (let [timestamp (edn/read-string (first entry))
        remainder (rest entry)
        severity (first remainder)
        remainder (rest remainder)
        text (first remainder)
        remainder (rest remainder)]
    (when (not (seq remainder))
      (when (instance? java.sql.Timestamp timestamp)
        )
      
)))

(defn raw->structured [[timestamp [severity text]]]
  "This isn't realistic from all sorts of different perspectives."
  (if (= severity :error)
    (p/error :error-message)
    (format "%s: %s\n%s\n" timestamp severity text)))

(defn parse-log-entry [entry]
  (if (valid-entry? text)
    (raw->structured entry)
    (p/restart-case [])))

(defn skip-log-entry [c]
  "Just skip entries that are wrong"
  (when-let [restart (p/find-restart :skip-log-entry)]
    (p/invoke-restart restart)))

(defn log-analyzer []
  "Simulate analyzing a collection of log files"
  (p/handler-bind [state :entering]
                  [[malformed-log-entry-error skip-log-entry]]
                  (doseq [log (find-all-logs)]
                    (analyze-log log))))

(facts "Malformed log handler")
