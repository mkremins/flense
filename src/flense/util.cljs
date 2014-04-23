(ns flense.util)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; generic helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn delete
  "Deletes the item at `idx` in the vector `v`."
  [v idx]
  (vec (concat (subvec v 0 idx) (subvec v (inc idx)))))

(defn fempty
  "Like `fnil`, but tests for empty collections instead of nil values."
  [f x]
  #(let [x (if (empty? %) x %)] (f x)))

(defn insert
  "Inserts `item` into vector `v` at `idx`, pushing whatever was already at
   `idx` one slot to the right."
  [v idx item]
  (apply conj (subvec v 0 idx) item (subvec v idx)))

(defn lconj
  "Prepends `item` to vector `v`."
  [v item]
  (vec (concat [item] v)))

(defn update
  "Like `update-in`, but takes a single key `k` as its second argument instead
   of a key sequence."
  [m k f & args]
  (apply (partial update-in m [k] f) args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; more app-specific helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
