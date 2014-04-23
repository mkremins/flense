(ns flense.zip
  (:refer-clojure :exclude [replace])
  (:require [flense.util :refer [lconj update]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; path movement
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- sibling [path n]
  (when (>= n 0) (conj (pop path) n)))

(defn- down* [path]
  (conj path 0))

(defn- left* [path]
  (when (seq path) (sibling path (dec (peek path)))))

(defn leftmost* [path]
  (when (seq path) (sibling path 0)))

(defn- right* [path]
  (when (seq path) (sibling path (inc (peek path)))))

(defn- up* [path]
  (when (seq path) (pop path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; path and zipper helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- full-path [path]
  (vec (interleave (repeat :children) path)))

(defn node [{:keys [tree path]}]
  (get-in tree (full-path path)))

(defn branch?
  "Tests whether `node` is a branch node (i.e. permitted to have children)."
  [node]
  (not (nil? (:children node))))

(defn- check
  "Tests whether `(:path loc)` points to an extant node in `(:tree loc)`,
   returning `loc` if the test passes and `nil` if it does not."
  [loc]
  (when (and (:path loc) (node loc)) loc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; simple zipper movement
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn down [loc]
  (check (update loc :path down*)))

(defn child [loc n]
  (check (-> loc down (update :path sibling n))))

(defn left [loc]
  (check (update loc :path left*)))

(defn leftmost [loc]
  (check (update loc :path leftmost*)))

(defn right [loc]
  (check (update loc :path right*)))

(defn up [loc]
  (check (update loc :path up*)))

(defn rightmost [loc]
  (when-let [parent (node (up loc))]
    (check (update loc :path
            sibling (-> parent :children count dec)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; wrapping zipper movement
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn down-or-stay
  "Returns the location immediately below `loc` (if it exists), or `loc` itself
   (if it doesn't)."
  [loc]
  (or (down loc) loc))

(defn left-or-wrap
  "Returns the location immediately to the left of `loc` (if it exists), or the
   rightmost sibling of `loc` (if it doesn't)."
  [loc]
  (or (left loc) (rightmost loc)))

(defn right-or-wrap
  "Returns the location immediately to the right of `loc` (if it exists), or
   the leftmost sibling of `loc` (if it doesn't)."
  [loc]
  (or (right loc) (leftmost loc)))

(defn up-or-stay
  "Returns the location immediately above `loc` (if it exists), or `loc` itself
   (if it doesn't)."
  [loc]
  (or (up loc) loc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; in-place zipper modification
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn replace [loc new-node]
  (check (assoc-in loc
          (lconj (full-path (:path loc)) :tree)
          new-node)))

(defn edit [loc f & args]
  (replace loc (apply f (node loc) args)))

(defn edit-parent [loc f & args]
  (when-let [parent-loc (up loc)]
    (apply edit parent-loc f (peek (:path loc)) args)))
