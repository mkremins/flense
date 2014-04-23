(ns flense.util)

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
