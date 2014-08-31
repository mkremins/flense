(ns flense.util)

(defn delete
  "Deletes the item at `idx` in the vector `v`."
  [v idx]
  (if (and (= (count v) 1) (= idx 0))
      (empty v)
      (vec (concat (subvec v 0 idx) (subvec v (inc idx))))))

(defn exchange
  "Swaps the values at keys `k1` and `k2` in associative data structure `m`."
  [m k1 k2]
  (assoc m k1 (get m k2) k2 (get m k1)))

(defn insert
  "Inserts `item` into vector `v` at `idx`, pushing whatever was already at
   `idx` one slot to the right."
  [v idx item]
  (apply conj (subvec v 0 idx) item (subvec v idx)))

(defn lconj
  "Prepends `item` to vector `v`."
  [v item]
  (vec (concat [item] v)))

(defn maybe
  "Returns `(f x)` if its value is non-nil, `x` otherwise."
  [f x]
  (or (f x) x))

(defn seek
  "Returns the first value in `coll` for which `pred` returns truthy."
  [pred coll]
  (first (filter pred coll)))

(defn update
  "Like `update-in`, but takes a single key `k` as its second argument instead
   of a key sequence."
  [m k f & args]
  (apply (partial update-in m [k] f) args))
