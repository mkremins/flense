(ns flense.edit.clojure
  (:require [flense.edit :refer [action find-placeholder]]
            [flense.parse :refer [form->tree]]
            [flense.util :refer [maybe update]]
            [xyzzy.core :as z]))

(def templates
  {"def"   '(def ... ...)
   "defn"  '(defn ... [...] ...)
   "defn-" '(defn- ... [...] ...)
   "fn"    '(fn [...] ...)
   "if"    '(if ... ... ...)
   "let"   '(let [... ...] ...)
   "when"  '(when ... ...)})

(action :clojure/expand-template
        :when (comp templates :text z/node)
        :edit #(-> % (z/edit (comp form->tree templates :text))
                 (find-placeholder z/next)))

(def ^:private dispatch-types
  {:map :set, :set :map,
   :seq :fn, :fn :seq,
   :string :regex, :regex :string})

(action :clojure/toggle-dispatch
        :when #(contains? dispatch-types (-> % z/node :type))
        :edit #(z/edit % update :type (partial maybe dispatch-types)))
