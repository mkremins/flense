(ns flense.parse
  (:require [cljs.reader :as rdr]
            [clojure.string :as string]
            [fs]))

(defn string->forms [string]
  (let [reader (rdr/push-back-reader string)]
    (loop [forms []]
      (if-let [form (rdr/read reader false nil false)]
              (recur (conj forms form))
              forms))))

(defn load-config [fpath]
  (rdr/read-string (fs/slurp fpath)))

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
  (merge
    {:form form :type (classify form)}
    (when (or (regex? form) (string? form))
      {:children [{:type :string-content :text form}]})
    (when (coll? form)
      {:children (mapv form->tree
                       (if (map? form)
                           (interpose (keys form) (vals form))
                           form))})))

(defn tree->str [tree]
  (if-let [form (:form tree)]
          (pr-str form)
          (let [delims
                (condp = (:type tree)
                  :fn     ["#("   ")"]
                  :map    ["{"    "}"]
                  :regex  ["#\"" "\""]
                  :seq    ["("    ")"]
                  :set    ["#{"   "}"]
                  :string ["\""  "\""]
                  :vec    ["["    "]"]
                          [""      ""])]
            (str (first delims)
                 (string/join " " (map tree->str (:children tree)))
                 (last delims)))))

(defn coll-node? [{:keys [type]}]
  (#{:fn :map :regex :seq :set :string :vec} type))

(defn stringish-node? [{:keys [type]}]
  (#{:regex :string} type))

(def placeholder
  (form->tree '...))

(defn placeholder-node? [{:keys [form]}]
  (= form (:form placeholder)))

(def expanders
  {'def  (form->tree '(def ... ...))
   'defn (form->tree '(defn ... [...] ...))
   'fn   (form->tree '(fn [...] ...))
   'if   (form->tree '(if ... ... ...))
   'let  (form->tree '(let [... ...] ...))
   'when (form->tree '(when ... ...))})

(defn expand-sexp [{:keys [type form] :as sexp}]
  (or (when (= type :symbol) (expanders form)) sexp))

(defn- parse-char [text]
  {:type :char :form (subs text 1)})

(defn- parse-keyword [text]
  {:type :keyword :form (keyword (subs text 1))})

(defn- parse-symbol-or-number [text]
  (let [number (js/parseFloat text)]
    (if (js/isNaN number)
        {:type :symbol :form (symbol text)}
        {:type :number :form number})))

(defn parse-atom [text]
  (let [init-ch (first text)]
    (cond
      (= text "false") {:type :bool :form false}
      (= text "nil")   {:type :nil  :form nil}
      (= text "true")  {:type :bool :form true}
      (= init-ch \\)   (parse-char text)
      (= init-ch \:)   (parse-keyword text)
      :else            (parse-symbol-or-number text))))
