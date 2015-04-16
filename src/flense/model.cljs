(ns flense.model
  "Functions for constructing, querying and manipulating xyzzy-compatible
   Clojure parse trees."
  (:refer-clojure
    :exclude [coll? fn? keyword? map? nil? number? seq? set? string? symbol?])
  (:require [cljs.reader :as rdr]
            [clojure.string :as str]
            [xyzzy.core :as z]))

(def placeholder {:type :symbol :text "..."})

(defn unwrap [m]
  (or (z/node m) m))

(defn type-pred [& types]
  (comp (set types) :type unwrap))

(def atom? (type-pred :bool :keyword :nil :number :symbol))
(def bool? (type-pred :bool))
(def coll? (type-pred :fn :map :seq :set :vec))
(def fn? (type-pred :fn))
(def keyword? (type-pred :keyword))
(def map? (type-pred :map))
(def nil? (type-pred :nil))
(def nonempty? (comp seq :children unwrap))
(def number? (type-pred :number))
(def placeholder? (comp (partial = "...") :text unwrap))
(def regex? (type-pred :regex))
(def seq? (type-pred :seq))
(def set? (type-pred :set))
(def string? (type-pred :string))
(def stringlike? (type-pred :regex :string))
(def symbol? (type-pred :symbol))
(def vec? (type-pred :vec))

(def editing? (every-pred stringlike? (comp :editing? unwrap)))

(defn symbol-named?
  ([name] (partial symbol-named? name))
  ([name m] (= (:text (unwrap m)) name)))

(defn deflike? [m]
  (let [{[head sym] :children :as node} (unwrap m)]
    (and (seq? node)
         (symbol? head) (= (subs (:text head) 0 3) "def")
         (symbol? sym))))

(defn fnlike? [m]
  (let [{[head] :children :as node} (unwrap m)]
    (and (seq? node)
         (#{"defmacro" "defmethod" "defn" "defn-" "fn"} (:text head)))))

(def fnlike-method?
  (every-pred seq? (comp vec? first :children unwrap)))

(defn letlike? [m]
  (let [{[head bvec] :children :as node} (unwrap m)]
    (and (seq? node)
         (#{"binding" "doseq" "for" "if-let" "if-some" "let" "loop"
            "when-first" "when-let" "when-some"} (:text head))
         (vec? bvec))))

(def lexical-scope? (some-fn fnlike? fnlike-method? letlike?))

(defn find-placeholder [loc direction]
  (z/find-next loc placeholder? direction))

(def prev-placeholder #(find-placeholder % z/prev))
(def next-placeholder #(find-placeholder % z/next))

;; conversions between raw strings, EDN data, parse tree nodes

(defn string->atom [string]
  {:text string
   :type (cond (#{"false" "true"} string) :bool
               (= string "nil") :nil
               (= (first string) \:) :keyword
               :else (let [number (js/parseFloat string)]
                       (if (js/isNaN number) :symbol :number)))})

(defn classify [x]
  (condp #(%1 %2) x
    (some-fn true? false?) :bool
    cljs.core/keyword? :keyword
    cljs.core/map? :map
    cljs.core/nil? :nil
    cljs.core/number? :number
    #(instance? js/RegExp %) :regex
    cljs.core/seq? :seq
    cljs.core/set? :set
    cljs.core/string? :string
    cljs.core/symbol? :symbol
    vector? :vec))

(defn form->tree
  "Returns the parse tree representation of a single Clojure `form`. A parse
  tree node is a map containing at minimum a `:type` keyword and either a
  `:text` string (if representing an atom) or a `:children` vector (if
  representing a collection)."
  [form]
  (let [type (classify form)]
    (merge {:type type}
      (case type
        (:bool :keyword :number :string :symbol) {:text (str form)}
        :nil {:text "nil"}
        (:seq :set :vec) {:children (mapv form->tree form)}
        :map {:children (mapv form->tree (interleave (keys form) (vals form)))}
        :regex {:text (.-source form)}))))

(defn annotate-paths
  "Given a parse `tree` representing a tree of Clojure forms, returns a copy of
  the tree with `:path` information attached to each node."
  [{:keys [children path] :or {path []} :as tree}]
  (cond-> tree (seq children)
          (assoc :children
                 (->> (map-indexed #(assoc %2 :path (conj path %1)) children)
                      (mapv annotate-paths)))))

(defn forms->document
  "Returns a document representing a seq of Clojure `forms`. A document is an
  xyzzy zipper over a Clojure parse tree, suitable for use as a Flense editor
  component's top-level state."
  [forms]
  {:path [0]
   :tree (annotate-paths {:children (mapv form->tree forms)})})

(defn tree->form [tree]
  (case (:type tree)
    (:bool :char :keyword :nil :number :symbol) (rdr/read-string (:text tree))
    :map (apply hash-map (map tree->form (:children tree)))
    :seq (map tree->form (:children tree))
    :set (set (map tree->form (:children tree)))
    :vec (mapv tree->form (:children tree))
    :string (:text tree)
    :regex (js/RegExp. (:text tree))))

(defn opener [type]
  (case type
    :fn "#(" :map "{" :seq "(" :set "#{" :vec "[" :string "\"" :regex "#\""))

(defn closer [type]
  (case type
    (:fn :seq) ")" (:map :set) "}" :vec "]" (:string :regex) "\""))

(defn tree->string [{:keys [type] :as tree}]
  (cond
    (coll? tree)
      (str (opener type)
           (str/join \space (map tree->string (:children tree)))
           (closer type))
    (stringlike? tree)
      (str (opener type) (:text tree) (closer type))
    :else
      (:text tree)))
