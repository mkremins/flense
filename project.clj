(defproject mkremins/flense "0.0-SNAPSHOT"
  :description "Experimental structural editor for Clojure code"
  :url "https://github.com/mkremins/flense"
  :license {:name "MIT License"
            :url "http://opensource.org/licenses/MIT"
            :distribution :repo}

  :dependencies
  [[org.clojure/clojure "1.6.0"]
   [org.clojure/clojurescript "0.0-2665"]
   [com.facebook/react "0.12.2.1"]
   [org.om/om "0.8.0"]
   [mkremins/xyzzy "0.3.3"]]

  :plugins
  [[lein-cljsbuild "1.0.4"]]

  :source-paths ["src"]

  :cljsbuild
  {:builds
   [{:source-paths ["src"]
     :compiler {:preamble ["react/react.js"]
                :output-to "target/flense.js"
                :source-map "target/flense.js.map"
                :optimizations :whitespace
                :pretty-print true}}]})
