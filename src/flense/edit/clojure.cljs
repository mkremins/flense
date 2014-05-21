(ns flense.edit.clojure
  (:require [flense.edit :refer [action]]
            [flense.parse :refer [form->tree]]
            [flense.util :refer [maybe update]]
            [xyzzy.core :as z]))

(def templates
  {"def"   (form->tree '(def ... ...))
   "defn"  (form->tree '(defn ... [...] ...))
   "defn-" (form->tree '(defn- ... [...] ...))
   "fn"    (form->tree '(fn [...] ...))
   "if"    (form->tree '(if ... ... ...))
   "let"   (form->tree '(let [... ...] ...))
   "when"  (form->tree '(when ... ...))})

(action :clojure/expand-template
        :when (comp templates :text z/node)
        :edit #(z/edit % (comp templates :text)))

(def ^:private dispatch-types
  {:map :set, :set :map,
   :seq :fn, :fn :seq,
   :string :regex, :regex :string})

(action :clojure/toggle-dispatch
        :when #(contains? dispatch-types (-> % z/node :type))
        :edit #(z/edit % update :type (partial maybe dispatch-types)))
