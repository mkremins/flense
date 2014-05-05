(ns flense.zip
  (:refer-clojure :exclude [find replace])
  (:require [flense.util :refer [insert lconj update]]))

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
;; walking zipper movement
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- walk [direction postwalk loc]
  (if-let [down-loc (down loc)]
          (postwalk down-loc)
          (loop [next-loc loc]
            (cond (direction next-loc) (direction next-loc)
                  (up next-loc) (recur (up next-loc))
                  :else nil))))

(def backward (partial walk left rightmost))
(def forward  (partial walk right leftmost))

(defn find
  "Returns a lazy sequence of locations following `loc` in `direction` (default
   `forward`) for which `(pred location)` returns truthy."
  ([loc pred]
    (find loc pred forward))
  ([loc pred direction]
    (->> loc
         (iterate direction)
         (take-while identity)
         (filter pred))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; wrapping zipper movement
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn left-or-wrap
  "Returns the location immediately to the left of `loc` (if it exists), the
   rightmost sibling of `loc` (if it doesn't), or `loc` itself (if `loc` is at
   the top of the tree)."
  [loc]
  (or (left loc) (rightmost loc) loc))

(defn right-or-wrap
  "Returns the location immediately to the right of `loc` (if it exists), the
   leftmost sibling of `loc` (if it doesn't), or `loc` itself (if `loc` is at
   the top of the tree)."
  [loc]
  (or (right loc) (leftmost loc) loc))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; node insertion and deletion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- insert-child* [parent i child]
  (update parent :children insert i child))

(defn- insert-rightmost* [parent _ child]
  (update parent :children conj child))

(defn insert-left [loc node]
  (-> loc
      (edit-parent insert-child* node)
      (child (-> loc :path peek))))

(defn insert-right [loc node]
  (if-let [right-loc (right loc)]
          (-> right-loc
              (edit-parent insert-child* node)
              (child (-> loc :path peek inc)))
          (-> loc (edit-parent insert-rightmost* node) down rightmost)))
