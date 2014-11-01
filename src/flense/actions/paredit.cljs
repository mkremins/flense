(ns flense.actions.paredit
  (:require [flense.model :as m]
            [flense.util :refer [exchange update]]
            [xyzzy.core :as z]))

(defn delete [loc]
  (let [{:keys [text] :as node} (z/node loc)]
    (cond
      (m/placeholder? node)
        (when (or (z/up (z/up loc))
                  (> (-> loc z/up z/node :children count) 1))
          (z/remove loc))
      (and (not (m/stringlike? node)) text (> (count text) 1))
        (z/replace loc (m/string->atom (subs text 0 (dec (count text)))))
      :else
        (z/replace loc m/placeholder))))

(defn delete-hard [loc]
  (if (m/placeholder-loc? loc)
    (when (or (z/up (z/up loc))
              (> (-> loc z/up z/node :children count) 1))
      (z/remove loc))
    (z/replace loc m/placeholder)))

(defn grow-left [loc]
  (when ((every-pred z/left m/collection-loc?) loc)
    (let [n   (-> loc :path peek dec)
          sib (-> loc z/left z/node)]
      (-> loc z/up (z/remove-child n) (z/child n) (z/insert-child 0 sib)))))

(defn grow-right [loc]
  (when ((every-pred z/right m/collection-loc?) loc)
    (let [n   (-> loc :path peek)
          sib (-> loc z/right z/node)]
      (-> loc z/up (z/remove-child (inc n)) (z/child n)
          (z/edit #(update % :children conj sib))))))

(defn insert-outside [loc]
  (when (z/up (z/up loc))
    (-> loc z/up (z/insert-right m/placeholder) z/right)))

(defn join-left [loc]
  (when ((every-pred m/collection-loc? (comp m/collection-loc? z/left)) loc)
    (let [n  (-> loc :path peek dec)
          cs (-> loc z/left z/node :children)]
      (-> loc
          (z/edit (fn [form] (update form :children #(vec (concat cs %)))))
          z/up (z/remove-child n) (z/child n)))))

(defn join-right [loc]
  (when ((every-pred m/collection-loc? (comp m/collection-loc? z/right)) loc)
    (let [n  (-> loc :path peek)
          cs (-> loc z/right z/node :children)]
      (-> loc
          (z/edit (fn [form] (update form :children #(vec (concat % cs)))))
          z/up (z/remove-child (inc n)) (z/child n)))))

(defn raise [loc]
  (when (z/up (z/up loc))
    (z/replace (z/up loc) (z/node loc))))

(defn shrink-left [loc]
  (when ((every-pred z/up m/collection-loc? m/nonempty-loc?) loc)
    (let [sib (-> loc z/down z/node)]
      (-> loc (z/remove-child 0) (z/insert-left sib)))))

(defn shrink-right [loc]
  (when ((every-pred z/up m/collection-loc? m/nonempty-loc?) loc)
    (let [sib (-> loc z/down z/rightmost z/node)]
      (-> loc (z/edit #(update % :children (comp vec butlast)))
          (z/insert-right sib)))))

(defn splice [loc]
  (when ((every-pred z/up m/collection-loc? m/nonempty-loc?) loc)
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

(defn- set-type [type loc]
  (when (m/collection-loc? loc)
    (z/edit loc assoc :type type)))

(defn- wrap-type [type loc]
  (z/down (z/edit loc #(-> {:type %2 :children [%1]}) type)))

(defn wrap-quote [loc]
  (when (m/atom-loc? loc)
    (z/edit loc assoc :type :string :editing? true)))

(def actions
  {:paredit/delete          delete
   :paredit/delete-hard     delete-hard
   :paredit/grow-left       grow-left
   :paredit/grow-right      grow-right
   :paredit/insert-left     #(-> % (z/insert-left m/placeholder) z/left)
   :paredit/insert-outside  insert-outside
   :paredit/insert-right    #(-> % (z/insert-right m/placeholder) z/right)
   :paredit/join-left       join-left
   :paredit/join-right      join-right
   :paredit/make-curly      (partial set-type :map)
   :paredit/make-round      (partial set-type :seq)
   :paredit/make-square     (partial set-type :vec)
   :paredit/raise           raise
   :paredit/shrink-left     shrink-left
   :paredit/shrink-right    shrink-right
   :paredit/splice          splice
   :paredit/split-left      split-left
   :paredit/split-right     split-right
   :paredit/swap-left       swap-left
   :paredit/swap-right      swap-right
   :paredit/wrap-curly      (partial wrap-type :map)
   :paredit/wrap-quote      wrap-quote
   :paredit/wrap-round      (partial wrap-type :seq)
   :paredit/wrap-square     (partial wrap-type :vec)})
