(ns flense.util)

(defn delete [v idx]
  (apply conj (subvec v 0 idx) (subvec v (inc idx))))

(defn insert [v idx item]
  (apply conj (subvec v 0 idx) item (subvec v idx)))

(defn update [m k f & args]
  (apply (partial update-in m [k] f) args))
