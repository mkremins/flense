(ns flense.edit.clojure
  (:refer-clojure :exclude [macroexpand macroexpand-1])
  (:require [flense.edit :refer [action find-placeholder]]
            [flense.parse :refer [form->tree tree->form]]
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

(def macros
  {'when (fn [test & body] `(if ~test (do ~@body) nil))})

(defn- macro-form? [form]
  (and (seq? form) (contains? macros (first form))))

(defn- macroexpand-1 [[sym & args]]
  (apply (macros sym) args))

(defn- macroexpand [form]
  (let [expanded (macroexpand-1 form)]
    (if (macro-form? expanded)
      (recur expanded)
      expanded)))

(action :clojure/expand-macro
        :when #(-> % z/node tree->form macro-form?)
        :edit #(z/edit % (comp form->tree macroexpand tree->form)))
