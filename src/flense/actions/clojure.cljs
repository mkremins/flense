(ns flense.actions.clojure
  (:refer-clojure :exclude [macroexpand macroexpand-1])
  (:require [flense.model :as m]
            [flense.util :refer [seek]]
            [xyzzy.core :as z]))

;; toggle dispatch reader macro

(def ^:private dispatch-types
  {:fn :seq, :seq :fn, :map :set, :set :map, :string :regex, :regex :string})

(defn toggle-dispatch [loc]
  (when (contains? dispatch-types (:type (z/node loc)))
    (z/update loc :type dispatch-types)))

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
              (z/assoc :collapsed-form node)))))

(defn collapse-macro [loc]
  (z/edit loc :collapsed-form))

;; find introduction of binding symbol

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

(defn find-scoped-declaration [loc sym-name]
  (loop [loc loc]
    ;; TODO doesn't work under certain shadowing conditions
    (when-let [loc' (z/up loc)]
      (or (seek (m/symbol-named? sym-name) (binding-locs loc))
          (recur loc')))))

(defn find-var-definition [loc var-name]
  (z/find-next-node
    (z/top loc)
    (every-pred m/deflike? (comp (m/symbol-named? var-name) second :children))
    z/next))

(defn find-introduction [loc]
  (when (m/symbol? loc)
    (let [sym-name (:text (z/node loc))]
      (or (find-scoped-declaration loc sym-name)
          (some-> loc (find-var-definition sym-name) z/down z/right)))))

;; rename binding symbol

(defn enclosing-scope
  "Returns the first loc above `loc` at which a new lexical scope is
  introduced, or nil if no such loc exists."
  [loc]
  (z/find-next-node loc m/lexical-scope? z/up))

(defn find-references
  "Returns a seq of locs that refer to the same binding as the binding symbol
  at `loc`. Obeys scoping rules."
  [loc]
  (let [loc (find-introduction loc)]
    ;; TODO probably doesn't work for top-level defs
    (some->> loc enclosing-scope z/descendants
             (filter (m/symbol-named? (:text (z/node loc))))
             (filter #(= (:path (find-introduction %)) (:path loc))))))

(defn rename-symbol
  "Renames the binding symbol at `loc` to `new-name`. Obeys scoping rules."
  [loc new-name]
  (let [new-sym (m/string->atom new-name)]
    (-> (reduce (fn [loc ref-path]
                  (z/replace (assoc loc :path ref-path) new-sym))
                loc (map :path (find-references loc)))
        (assoc :path (:path loc)))))
