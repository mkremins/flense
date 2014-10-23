(ns flense.actions.clojure
  (:refer-clojure :exclude [macroexpand macroexpand-1])
  (:require [flense.model :refer [find-placeholder form->tree tree->form]]
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

;; toggle dispatch reader macro

(def ^:private dispatch-types
  {:fn :seq, :seq :fn, :map :set, :set :map, :string :regex, :regex :string})

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

;; jump to definition

(defn- find-def-form [loc sym-name]
  (z/find-next-node loc
    #(let [[head sym] (:children %)]
       (and (:text head)
            (= (subs (:text head) 0 3) "def")
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

;; define actions map for export

(def actions
  {:clojure/expand-template
   #(when-let [template (templates (:text (z/node %)))]
      (-> % (z/replace (form->tree template)) (find-placeholder z/next)))

   :clojure/toggle-dispatch
   #(when (contains? dispatch-types (z/node (type %)))
      (z/edit % update :type dispatch-types))

   :clojure/expand-macro
   #(when (-> % z/node tree->form macro-form?)
      (-> % (z/edit (comp form->tree macroexpand tree->form))
            (z/edit assoc :collapsed-form (z/node %))))

   :clojure/collapse-macro
   #(z/edit % :collapsed-form)

   :clojure/jump-to-definition
   #(when (= (:type (z/node %)) :symbol)
      (find-definition % (:text (z/node %))))})
