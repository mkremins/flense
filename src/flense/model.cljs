(ns flense.model
  "Functions for constructing, querying and manipulating xyzzy-compatible
   Clojure parse trees."
  (:require [cljs.reader :as rdr]
            [clojure.string :as str]
            [xyzzy.core :as z]))

;; parse tree nodes

(def placeholder {:type :symbol :text "..."})

(defn atom? [node]
  (#{:bool :char :keyword :nil :number :symbol} (:type node)))

(defn collection? [node]
  (#{:fn :map :seq :set :vec} (:type node)))

(defn nonempty? [node]
  (seq (:children node)))

(defn placeholder? [node]
  (= (:text node) "..."))

(defn stringlike? [node]
  (#{:regex :string} (:type node)))

;; parse tree zippers

(def atom-loc?        (comp atom? z/node))
(def collection-loc?  (comp collection? z/node))
(def nonempty-loc?    (comp nonempty? z/node))
(def placeholder-loc? (comp placeholder? z/node))
(def stringlike-loc?  (comp stringlike? z/node))

(defn find-placeholder [loc direction]
  (z/find-next loc placeholder-loc? direction))

(def prev-placeholder #(find-placeholder % z/prev))
(def next-placeholder #(find-placeholder % z/next))

;; conversions between raw strings, EDN data, parse tree nodes

(defn- string->forms [string]
  (let [reader (rdr/push-back-reader string)]
    (loop [forms []]
      (if-let [form (rdr/read reader false nil false)]
        (recur (conj forms form))
        forms))))

(defn string->atom [string]
  {:text string
   :type (cond (#{"false" "true"} string) :bool
               (= string "nil") :nil
               (= (first string) \:) :keyword
               :else (let [number (js/parseFloat string)]
                       (if (js/isNaN number) :symbol :number)))})

(defn- bool? [x]
  (or (true? x) (false? x)))

(defn- regex? [x]
  (instance? js/RegExp x))

(defn classify [x]
  (condp apply [x]
    bool?    :bool
    keyword? :keyword
    map?     :map
    nil?     :nil
    number?  :number
    regex?   :regex
    seq?     :seq
    set?     :set
    string?  :string
    symbol?  :symbol
    vector?  :vec))

(defn form->tree [form]
  (let [type (classify form)]
    (merge {:type type}
      (case type
        (:bool :keyword :number :string :symbol) {:text (str form)}
        :nil {:text "nil"}
        (:seq :set :vec) {:children (mapv form->tree form)}
        :map {:children (mapv form->tree (interleave (keys form) (vals form)))}
        :regex {:text (.-source form)}))))

(defn forms->document [forms]
  {:path [0]
   :tree {:children (mapv form->tree forms)}})

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
    (collection? tree)
    (str (opener type)
         (str/join \space (map tree->string (:children tree)))
         (closer type))
    (stringlike? tree)
    (str (opener type) (:text tree) (closer type))
    :else
    (:text tree)))
