(ns flense.util)

(defn exchange
  "Swaps the values at keys `k1` and `k2` in associative data structure `m`."
  [m k1 k2]
  (assoc m k1 (get m k2) k2 (get m k1)))

(defn seek
  "Returns the first value in `coll` for which `pred` returns truthy."
  [pred coll]
  (first (filter pred coll)))

(defn update
  "Like `update-in`, but takes a single key `k` as its second argument instead
   of a key sequence."
  [m k f & args]
  (apply (partial update-in m [k] f) args))
