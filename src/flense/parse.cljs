(ns flense.parse
  (:require [cljs.reader :as rdr]
            [clojure.string :as string]
            [fs]))

(defn- regex? [x]
  (instance? js/RegExp x))

(defn classify [x]
  (cond (false?   x) :bool
        (true?    x) :bool
        (keyword? x) :keyword
        (map?     x) :map
        (nil?     x) :nil
        (number?  x) :number
        (regex?   x) :regex
        (seq?     x) :seq
        (set?     x) :set
        (string?  x) :string
        (symbol?  x) :symbol
        (vector?  x) :vec))

(defn form->tree [form]
  (let [type (classify form)]
    (merge {:type type}
     (condp contains? type
      #{:bool :keyword :number :symbol}
       {:text (str form)}
      #{:nil}
       {:text "nil"}
      #{:seq :set :vec}
       {:children (mapv form->tree form)}
      #{:map}
       {:children (mapv form->tree (interleave (keys form) (vals form)))}
      #{:regex :string}
       {:children [{:type :string-content :text form}]}))))

(defn- string->forms [string]
  (let [reader (rdr/push-back-reader string)]
    (loop [forms []]
      (if-let [form (rdr/read reader false nil false)]
              (recur (conj forms form))
              forms))))

(defn load-config [fpath]
  (rdr/read-string (fs/slurp fpath)))

(defn load-source [fpath]
  {:path [0]
   :tree {:children (->> (fs/slurp fpath) string->forms (mapv form->tree))}})

(defn coll-node? [{:keys [type]}]
  (#{:fn :map :regex :seq :set :string :vec} type))

(defn stringish-node? [{:keys [type]}]
  (#{:regex :string} type))

(def placeholder
  (form->tree '...))

(defn placeholder-node? [{:keys [text]}]
  (= text "..."))

(defn tree->str [tree]
  (if (coll-node? tree)
      (let [delims
            ({:fn     ["#("   ")"]
              :map    ["{"    "}"]
              :regex  ["#\"" "\""]
              :seq    ["("    ")"]
              :set    ["#{"   "}"]
              :string ["\""  "\""]
              :vec    ["["    "]"]} (:type tree))]
        (str (first delims)
             (string/join " " (map tree->str (:children tree)))
             (last delims)))
      (:text tree)))

(def expanders
  {"def"  (form->tree '(def ... ...))
   "defn" (form->tree '(defn ... [...] ...))
   "fn"   (form->tree '(fn [...] ...))
   "if"   (form->tree '(if ... ... ...))
   "let"  (form->tree '(let [... ...] ...))
   "when" (form->tree '(when ... ...))})

(defn expand-sexp [{:keys [type text] :as sexp}]
  (or (when (= type :symbol) (expanders text)) sexp))

(defn- symbol-or-number [text]
  (let [number (js/parseFloat text)]
    (if (js/isNaN number) :symbol :number)))

(defn parse-token [text]
  (let [init-ch (first text)]
    {:text text
     :type (cond (#{"false" "true"} text) :bool
                 (= text "nil") :nil
                 (= init-ch \\) :char
                 (= init-ch \:) :keyword
                 :else (symbol-or-number text))}))
