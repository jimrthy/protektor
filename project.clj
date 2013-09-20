(defproject jimrthy/protektor "0.1.0-SNAPSHOT"
  :description "Common Lisp-ish style condition handling for clojure"
  :url "https://github.com/jimrthy/protektor"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :min-lein-version "2.0.0"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/tools.trace "0.7.6"]]
  :profiles {:dev {:dependencies [[midje "1.5.1"]
                                  [org.clojure/tools.namespace "0.2.3"]
                                  [org.clojure/java.classpath "0.2.0"]]
                   :source-paths ["dev"]}}
  :pedantic? :abort
  :main protektor.core)
