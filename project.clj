(defproject flense "0.1.0-SNAPSHOT"
  :dependencies
  [[org.clojure/clojure "1.6.0"]
   [org.clojure/clojurescript "0.0-2322"]
   [org.clojure/core.async "0.1.338.0-5c5012-alpha"]
   [com.facebook/react "0.11.1"]
   [om "0.7.1"]
   [spellhouse/phalanges "0.1.3"]
   [mkremins/fs "0.3.0"]
   [mkremins/xyzzy "0.2.1"]]

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
