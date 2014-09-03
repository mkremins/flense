(ns flense.edit.clojure
  (:refer-clojure :exclude [macroexpand macroexpand-1])
  (:require [clojure.string :as str]
            [flense.edit :refer [action find-placeholder]]
            [flense.model :refer [form->tree tree->form]]
            [flense.util :refer [maybe seek update]]
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

(action :clojure/expand-template
        :when (comp templates :text z/node)
        :edit #(-> % (z/edit (comp form->tree templates :text))
                 (find-placeholder z/next)))

;; toggle dispatch reader macro

(def ^:private dispatch-types
  {:map :set, :set :map,
   :seq :fn, :fn :seq,
   :string :regex, :regex :string})

(action :clojure/toggle-dispatch
        :when #(contains? dispatch-types (-> % z/node :type))
        :edit #(z/edit % update :type (partial maybe dispatch-types)))

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

(action :clojure/expand-macro
        :when #(-> % z/node tree->form macro-form?)
        :edit #(-> % (z/edit (comp form->tree macroexpand tree->form))
                     (z/edit assoc :collapsed-form (z/node %))))

(action :clojure/collapse-macro
        :when #(-> % z/node :collapsed-form)
        :edit #(z/edit % :collapsed-form))

;; jump to definition

(defn- find-def-form [loc sym-name]
  (z/find-next-node loc
    #(let [[head sym] (:children %)]
       (and (= (str/join (take 3 (:text head))) "def")
            (= (:text sym) sym-name)))
    z/next))

(defn- bsym-locs [loc]
  (case (:type (z/node loc))
    :symbol [loc]
    (:map :vec) (mapcat bsym-locs (z/children loc))
    []))

(defmulti binding-locs
  (fn [loc] (some-> loc z/node :children first :text symbol)))

(defmethod binding-locs :default [_] ())

(doseq [fnlike '[defmacro defmethod defn defn- fn]]
  (defmethod binding-locs fnlike [loc]
    (->> (seek #(= (:type (z/node %)) :vec) (z/children loc))
         z/children
         (mapcat bsym-locs))))

(doseq [letlike '[binding doseq for if-let if-some let loop when-first when-let when-some]]
  (defmethod binding-locs letlike [loc]
    (when (= (-> loc z/node :children second :type) :vec)
      (->> (z/children (-> loc z/down z/right))
           (partition 2)
           (map first)
           (mapcat bsym-locs)))))

(defn- find-definition [loc sym-name]
  (if-let [loc' (z/up loc)]
    (or (seek #(= (:text (z/node %)) sym-name) (binding-locs loc))
        (recur loc' sym-name))
    (-> (find-def-form loc sym-name) z/down z/right)))

(action :clojure/jump-to-definition
        :when #(= (:type (z/node %)) :symbol)
        :edit #(or (find-definition % (:text (z/node %))) %))
