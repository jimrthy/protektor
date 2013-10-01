(ns protektor.log-test
  (require [protektor.core :as p]
           [clojure.edn :as edn]
           [slingshot.slingshot :refer [throw+]])
  (use midje.sweet))

;;; Inspired by _Practical Common Lisp_
(defn find-all-logs []
  "Pretend to pull back a bunch of logs from somewhere.
Realistically, these should be lines of text. Although it's almost
definitely better to save logs to a database instead of just a txt 
file.

The first should be a timestamp, but that really isn't relevant
for what I'm trying to do here."
  [[true [:normal "Entry One"]]
   [true [:error "Fail"]]
   [true [:warn "Something special should probably happen with this"]]
   [false [:normal "This shouldn't work either"]]
   [true [:normal "This shouldn't work"] "extra: causes error"]])

(defn valid-entry? [entry]
  "This approach is stupid and more than a little pointless...but
I don't have any actual interest in building a 'real' parser."
  (if (= 3 (count entry))
    (boolean (first entry))
    (throw+ :gibberish)))

(defn raw->structured [[timestamp [severity text]]]
  "This isn't realistic from all sorts of different perspectives."
  (if (= severity :error)
    (throw+ :error-message)
    (format "%s: %s\n%s\n" timestamp severity text)))

(defn skip-log-entry [_])

(defn parse-log-entry [entry]
  "This is where the interesting pieces happen"
  (if (valid-entry? entry)
    (raw->structured entry)
    (p/restart-case
     [local 1
      local2 :a
      local3 :b]
     (s/throw+ {:type :malformed-log-entry-error :text text})
     (:skip-log-entry skip-log-entry)
     (:use-value [value] value)
     (:reparse-entry [fixed-text]
                    (parse-log-entry fixed-text)))))

(defn analyze-entry [entry]
  ;; This should pretend to do something useful with whatever parse-log-entry generated.
  (println entry))

(defn analyze-log [log]
  (doseq [entry (parse-log-entry log)]
    (analyze-entry entry)))

(defn skip-log-entry [c]
  "Just skip entries that are wrong"
  (when-let [restart (p/find-restart :skip-log-entry)]
    (p/invoke-restart restart)))

(defn log-analyzer []
  "Simulate analyzing a collection of log files"
  (p/handler-bind [state :entering]
                  ;; Bind handlers
                  [[:malformed-log-entry-error skip-log-entry]
                   [:error-entry (fn [c] (p/use-value
                                          {:time-stamp "!!!!!" 
                                           :severity "ERROR"
                                           :text (text c)}))]
                   [:gibberish (fn [c] (p/invoke-restart reparse-entry))]]
                  (doseq [log (find-all-logs)]
                    (analyze-log log))))

;;; What on earth can I do here that might be useful?
(facts "Malformed log handler"
       (fact "Overall" (log-analyzer) => nil?))
