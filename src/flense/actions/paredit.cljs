(ns flense.actions.paredit
  (:require [flense.model :as m]
            [flense.util :refer [exchange update]]
            [xyzzy.core :as z]))

(defn delete [loc]
  (if (m/placeholder? loc)
    (when (or (z/up (z/up loc))
              (> (-> loc z/up z/node :children count) 1))
      (z/remove loc))
    (z/replace loc m/placeholder)))

(defn grow-left [loc]
  (when ((every-pred z/left m/coll?) loc)
    (let [n   (-> loc :path peek dec)
          sib (-> loc z/left z/node)]
      (-> loc z/up (z/remove-child n) (z/child n) (z/insert-child 0 sib)))))

(defn grow-right [loc]
  (when ((every-pred z/right m/coll?) loc)
    (let [n   (-> loc :path peek)
          sib (-> loc z/right z/node)]
      (-> loc z/up (z/remove-child (inc n)) (z/child n)
          (z/edit #(update % :children conj sib))))))

(defn insert-left [loc]
  (-> loc (z/insert-left m/placeholder) z/left))

(defn insert-outside [loc]
  (when (z/up (z/up loc))
    (-> loc z/up (z/insert-right m/placeholder) z/right)))

(defn insert-right [loc]
  (-> loc (z/insert-right m/placeholder) z/right))

(defn join-left [loc]
  (when ((every-pred m/coll? (comp m/coll? z/left)) loc)
    (let [n  (-> loc :path peek dec)
          cs (-> loc z/left z/node :children)]
      (-> loc
          (z/edit (fn [form] (update form :children #(vec (concat cs %)))))
          z/up (z/remove-child n) (z/child n)))))

(defn join-right [loc]
  (when ((every-pred m/coll? (comp m/coll? z/right)) loc)
    (let [n  (-> loc :path peek)
          cs (-> loc z/right z/node :children)]
      (-> loc
          (z/edit (fn [form] (update form :children #(vec (concat % cs)))))
          z/up (z/remove-child (inc n)) (z/child n)))))

(defn- make-type [type loc]
  (when (m/coll? loc)
    (z/edit loc assoc :type type)))

(def make-map (partial make-type :map))
(def make-seq (partial make-type :seq))
(def make-vec (partial make-type :vec))

(defn next-placeholder [loc]
  (m/find-placeholder loc z/next))

(defn prev-placeholder [loc]
  (m/find-placeholder loc z/prev))

(defn raise [loc]
  (when (z/up (z/up loc))
    (z/replace (z/up loc) (z/node loc))))

(defn shrink-left [loc]
  (when ((every-pred z/up m/coll? m/nonempty?) loc)
    (let [sib (-> loc z/down z/node)]
      (-> loc (z/remove-child 0) (z/insert-left sib)))))

(defn shrink-right [loc]
  (when ((every-pred z/up m/coll? m/nonempty?) loc)
    (let [sib (-> loc z/down z/rightmost z/node)]
      (-> loc (z/edit #(update % :children (comp vec butlast)))
          (z/insert-right sib)))))

(defn splice [loc]
  (when ((every-pred z/up m/coll? m/nonempty?) loc)
    (let [n  (-> loc :path peek)
          cs (-> loc z/node :children)]
      (-> loc z/up
          (z/edit #(-> % (assoc-in [:children n] cs)
                         (update :children (comp vec flatten))))
          (z/child n)))))

(defn split-left [loc]
  (when (z/up (z/up loc))
    (let [split (-> loc :path peek)
          node  (-> loc z/up z/node)
          [ls rs] (map vec (split-at split (:children node)))]
      (-> loc z/up
          (z/edit assoc :children rs)
          (z/insert-left (assoc node :children ls))
          z/down))))

(defn split-right [loc]
  (when (z/up (z/up loc))
    (let [split (-> loc :path peek inc)
          node  (-> loc z/up z/node)
          [ls rs] (map vec (split-at split (:children node)))]
      (-> loc z/up
          (z/edit assoc :children ls)
          (z/insert-right (assoc node :children rs))
          z/down z/rightmost))))

(defn swap-left [loc]
  (when (z/left loc)
    (let [i (-> loc :path peek) j (dec i)]
      (-> loc z/up (z/edit update :children exchange i j) (z/child j)))))

(defn swap-right [loc]
  (when (z/right loc)
    (let [i (-> loc :path peek) j (inc i)]
      (-> loc z/up (z/edit update :children exchange i j) (z/child j)))))

(defn- wrap-type [type loc]
  (z/down (z/edit loc #(-> {:type type :children [%]}))))

(def wrap-map (partial wrap-type :map))
(def wrap-seq (partial wrap-type :seq))
(def wrap-vec (partial wrap-type :vec))
