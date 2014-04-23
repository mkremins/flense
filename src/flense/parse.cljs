(ns flense.parse)

(defn classify [x]
  (cond (false?   x) :bool
        (true?    x) :bool
        (keyword? x) :keyword
        (map?     x) :map
        (nil?     x) :nil
        (number?  x) :number
        (seq?     x) :seq
        (set?     x) :set
        (string?  x) :string
        (symbol?  x) :symbol
        (vector?  x) :vec))

(defn form->tree [form]
  (merge
    {:form form :type (classify form)}
    (when (coll? form)
      {:children (mapv form->tree
                       (if (map? form)
                           (interpose (keys form) (vals form))
                           form))})))

(defn coll-node? [{:keys [type]}]
  (#{:fn :map :seq :set :vec} type))

(def placeholder
  (form->tree '...))

(defn placeholder-node? [{:keys [form]}]
  (= form (:form placeholder)))

(defn- parse-char [text]
  {:type :char :form (subs text 1)})

(defn- parse-keyword [text]
  {:type :keyword :form (keyword (subs text 1))})

(defn- parse-regex [text]
  {:type :regex :form (re-pattern (subs text 2 (dec (count text))))})

(defn- parse-string [text]
  {:type :string :form (subs text 1 (dec (count text)))})

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
      (= init-ch \#)   (parse-regex text)
      (= init-ch \")   (parse-string text)
      :else            (parse-symbol-or-number text))))
