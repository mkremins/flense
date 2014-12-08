(ns flense.actions.clojure
  (:refer-clojure :exclude [macroexpand macroexpand-1])
  (:require [flense.model :as m]
            [flense.util :refer [seek update]]
            [xyzzy.core :as z]))

;; expand templates

(def templates
  {"def"   '(def ... ...)
   "defn"  '(defn ... [...] ...)
   "defn-" '(defn- ... [...] ...)
   "fn"    '(fn [...] ...)
   "if"    '(if ... ... ...)
   "let"   '(let [... ...] ...)
   "when"  '(when ... ...)})

(defn expand-template [loc]
  (when-let [template (templates (:text (z/node loc)))]
    (-> loc (z/replace (m/form->tree template)) (m/find-placeholder z/next))))

;; toggle dispatch reader macro

(def ^:private dispatch-types
  {:fn :seq, :seq :fn, :map :set, :set :map, :string :regex, :regex :string})

(defn toggle-dispatch [loc]
  (when (contains? dispatch-types (:type (z/node loc)))
    (z/edit loc update :type dispatch-types)))

;; expand and collapse macro forms

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

(defn expand-macro [loc]
  (let [node (z/node loc)]
    (when (-> node m/tree->form macro-form?)
      (-> loc (z/edit (comp m/form->tree macroexpand m/tree->form))
              (z/edit assoc :collapsed-form node)))))

(defn collapse-macro [loc]
  (z/edit loc :collapsed-form))

;; jump to definition

(defn- find-def-form [loc sym-name]
  (z/find-next-node loc
    (every-pred m/deflike? (comp #{sym-name} :text second :children))
    z/next))

(defn- bsym-locs [loc]
  (case (:type (z/node loc))
    :symbol [loc]
    (:map :vec) (mapcat bsym-locs (z/children loc))
    []))

(defn binding-locs [loc]
  (condp #(%1 %2) loc
    m/fnlike?
      (->> loc z/children (seek #(= (:type (z/node %)) :vec))
           z/children (mapcat bsym-locs))
    m/fnlike-method?
      (->> loc z/children first z/children (mapcat bsym-locs))
    m/letlike?
      (->> loc z/down z/right z/children
           (partition 2) (map first) (mapcat bsym-locs))
    ()))

(defn collect-binding-locs [loc]
  (loop [loc loc locs ()]
    (if-let [loc' (z/up loc)]
      (recur loc' (concat locs (binding-locs loc')))
      locs)))

(defn jump-to-definition [loc]
  (when (m/symbol? loc)
    (let [sym-name (:text (z/node loc))]
      (loop [loc loc]
        (if-let [loc' (z/up loc)]
          (or (seek #(= (:text (z/node %)) sym-name) (binding-locs loc))
              (recur loc'))
          (some-> (find-def-form loc sym-name) z/down z/right))))))
