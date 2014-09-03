(ns flense.actions.paredit
  (:require [flense.actions :refer [defaction]]
            [flense.model
             :refer [atom-loc? collection-loc? nonempty-loc? placeholder
                     placeholder-loc?]]
            [flense.util :refer [exchange update]]
            [xyzzy.core :as z]))

(defaction :paredit/grow-left
  :when #(and (z/left %) (collection-loc? %))
  :edit (fn [loc]
          (let [n   (-> loc :path peek dec)
                sib (-> loc z/left z/node)]
            (-> loc z/up
                (z/remove-child n) (z/child n)
                (z/insert-child 0 sib)))))

(defaction :paredit/grow-right
  :when #(and (z/right %) (collection-loc? %))
  :edit (fn [loc]
          (let [n   (-> loc :path peek)
                sib (-> loc z/right z/node)]
            (-> loc z/up (z/remove-child (inc n)) (z/child n)
                (z/edit #(update % :children conj sib))))))

(defaction :paredit/insert-left
  :when z/up :edit #(-> % (z/insert-left placeholder) z/left))

(defaction :paredit/insert-outside
  :when (comp z/up z/up)
  :edit #(-> % z/up (z/insert-right placeholder) z/right))

(defaction :paredit/insert-right
  :when z/up :edit #(-> % (z/insert-right placeholder) z/right))

(defaction :paredit/join-left
  :when #(and (collection-loc? %) (-> % z/left collection-loc?))
  :edit (fn [loc]
          (let [n  (-> loc :path peek dec)
                cs (-> loc z/left z/node :children)]
            (-> loc
                (z/edit (fn [form] (update form :children #(vec (concat cs %)))))
                z/up (z/remove-child n) (z/child n)))))

(defaction :paredit/join-right
  :when #(and (collection-loc? %) (-> % z/right collection-loc?))
  :edit (fn [loc]
          (let [n  (-> loc :path peek)
                cs (-> loc z/right z/node :children)]
            (-> loc
                (z/edit (fn [form] (update form :children #(vec (concat % cs)))))
                z/up (z/remove-child (inc n)) (z/child n)))))

(defaction :paredit/make-curly
  :when collection-loc? :edit #(z/edit % assoc :type :map))

(defaction :paredit/make-round
  :when collection-loc? :edit #(z/edit % assoc :type :seq))

(defaction :paredit/make-square
  :when collection-loc? :edit #(z/edit % assoc :type :vec))

(defaction :paredit/raise
  :when z/up :edit #(-> % z/up (z/replace (z/node %))))

(defaction :paredit/remove
  :when z/up
  :edit #(if (placeholder-loc? %)
             (if (and (not (-> % z/up z/up))
                      (= (-> % z/up z/node :children count) 1))
                 %
                 (z/remove %))
             (z/replace % placeholder)))

(defaction :paredit/shrink-left
  :when #(and (z/up %) (collection-loc? %) (nonempty-loc? %))
  :edit (fn [loc]
          (let [sib (-> loc z/down z/node)]
            (-> loc (z/remove-child 0) (z/insert-left sib)))))

(defaction :paredit/shrink-right
  :when #(and (z/up %) (collection-loc? %) (nonempty-loc? %))
  :edit (fn [loc]
          (let [sib (-> loc z/down z/rightmost z/node)]
            (-> loc (z/edit #(update % :children (comp vec butlast)))
                (z/insert-right sib)))))

(defaction :paredit/splice
  :when #(and (z/up %) (collection-loc? %) (nonempty-loc? %))
  :edit (fn [loc]
          (let [n  (-> loc :path peek)
                cs (-> loc z/node :children)]
            (-> loc z/up
                (z/edit #(-> % (assoc-in [:children n] cs)
                               (update :children (comp vec flatten))))
                (z/child n)))))

(defaction :paredit/split-left
  :when (comp z/up z/up)
  :edit (fn [loc]
          (let [split (-> loc :path peek)
                node  (-> loc z/up z/node)
                [ls rs] (map vec (split-at split (:children node)))]
            (-> loc z/up
                (z/edit assoc :children rs)
                (z/insert-left (assoc node :children ls))
                z/down))))

(defaction :paredit/split-right
  :when (comp z/up z/up)
  :edit (fn [loc]
          (let [split (-> loc :path peek inc)
                node  (-> loc z/up z/node)
                [ls rs] (map vec (split-at split (:children node)))]
            (-> loc z/up
                (z/edit assoc :children ls)
                (z/insert-right (assoc node :children rs))
                z/down z/rightmost))))

(defaction :paredit/swap-left
  :when z/left
  :edit (fn [loc]
          (let [i (-> loc :path peek) j (dec i)]
            (-> loc z/up (z/edit update :children exchange i j)
                (z/child j)))))

(defaction :paredit/swap-right
  :when z/right
  :edit (fn [loc]
          (let [i (-> loc :path peek) j (inc i)]
            (-> loc z/up (z/edit update :children exchange i j)
                (z/child j)))))

(defn- wrap [form type]
  {:type type :children [form]})

(defaction :paredit/wrap-curly
  :edit #(-> % (z/edit wrap :map) z/down))

(defaction :paredit/wrap-quote
  :when atom-loc?
  :edit #(z/edit % assoc :type :string :editing? true))

(defaction :paredit/wrap-round
  :edit #(-> % (z/edit wrap :seq) z/down))

(defaction :paredit/wrap-square
  :edit #(-> % (z/edit wrap :vec) z/down))
