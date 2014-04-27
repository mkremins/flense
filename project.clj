(defproject flense "0.1.0-SNAPSHOT"
  :dependencies
  [[org.clojure/clojure "1.6.0"]
   [org.clojure/clojurescript "0.0-2202"]
   [om "0.6.1"]
   [mkremins/fs "0.1.0"]]

  :plugins
  [[lein-cljsbuild "1.0.3"]]

  :source-paths ["src"]

  :cljsbuild
  {:builds
   [{:source-paths ["src"]
     :compiler {:output-to "target/flense.js"
                :optimizations :whitespace
                :pretty-print true}}]})
