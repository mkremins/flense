(defproject mkremins/flense "0.0-SNAPSHOT"
  :description "Experimental structural editor for Clojure code"
  :url "https://github.com/mkremins/flense"
  :license {:name "MIT License"
            :url "http://opensource.org/licenses/MIT"
            :distribution :repo}

  :dependencies
  [[org.clojure/clojure "1.6.0"]
   [org.clojure/clojurescript "0.0-2341"]
   [org.clojure/core.async "0.1.346.0-17112a-alpha"]
   [com.facebook/react "0.11.2"]
   [om "0.7.3"]
   [mkremins/xyzzy "0.3.0"]]

  :plugins
  [[lein-cljsbuild "1.0.3"]]

  :source-paths ["src"]

  :cljsbuild
  {:builds
   [{:source-paths ["src"]
     :compiler {:preamble ["react/react.js"]
                :output-to "target/flense.js"
                :source-map "target/flense.js.map"
                :optimizations :whitespace
                :pretty-print true}}]})
