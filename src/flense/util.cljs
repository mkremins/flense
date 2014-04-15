(ns flense.util)

(defn delete [v idx]
  (apply conj (subvec v 0 idx) (subvec v (inc idx))))

(defn insert [v idx item]
  (apply conj (subvec v 0 idx) item (subvec v idx)))

(defn update [m k f & args]
  (apply (partial update-in m [k] f) args))

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
