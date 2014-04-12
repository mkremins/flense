(ns flense.util)

(defn insert [v idx item]
  (apply conj (subvec v 0 idx) item (subvec v idx)))
