(ns flense.zip)

(defn sibling* [path n]
  (when (seq path) (conj (pop path) n)))

(defn down* [path]
  (conj path 0))

(defn left* [path]
  (when (seq path)
    (let [n (peek path)]
      (when (pos? n) (sibling* path (dec n))))))

(defn right* [path]
  (when (seq path)
    (sibling* path (inc (peek path)))))

(defn up* [path]
  (when (seq path) (pop path)))

(defn- full-path [path]
  (vec (if (seq path)
           (concat [:children] (interpose :children path))
           path)))

(defn assoc-path [tree path value]
  (assoc-in tree (full-path path) value))

(defn get-path [tree path]
  (get-in tree (full-path path)))

(defn update-path [tree path f & args]
  (let [update (partial update-in tree (full-path path) f)]
    (apply update args)))

(defn- insert [v idx item]
  (apply conj (subvec v 0 idx) item (subvec v idx)))

(defn insert-right [tree path value]
  (if-let [parent-path (up* path)]
    (update-in tree (conj (full-path parent-path) :children)
               insert (inc (peek path)) value)
    tree))
