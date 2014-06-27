(ns flense.parse
  (:require [cljs.reader :as rdr]
            [clojure.string :as string]
            [fs]))

(defn- regex? [x]
  (instance? js/RegExp x))

(defn classify [x]
  (condp apply [x]
    false?   :bool
    true?    :bool
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
