(ns flense.edit.paredit
  (:require [flense.edit
             :refer [action coll-loc? nonempty-loc? placeholder
                     placeholder-loc? string-content-loc? token-loc?]]
            [flense.util :refer [update]]
            [xyzzy.core :as z]))

(action :paredit/grow-left
        :when #(and (z/left %) (coll-loc? %))
        :edit (fn [loc]
                (let [n   (-> loc :path peek dec)
                      sib (-> loc z/left z/node)]
                  (-> loc z/up
                      (z/remove-child n) (z/child n)
                      (z/insert-child 0 sib)))))

(action :paredit/grow-right
        :when #(and (z/right %) (coll-loc? %))
        :edit (fn [loc]
                (let [n   (-> loc :path peek)
                      sib (-> loc z/right z/node)]
                  (-> loc z/up
                      (z/remove-child (inc n)) (z/child n)
                      z/down z/rightmost (z/insert-right sib) z/up))))

(action :paredit/insert-left
        :when z/up :edit #(-> % (z/insert-left placeholder) z/left))

(action :paredit/insert-outside
        :when (comp z/up z/up)
        :edit #(-> % z/up (z/insert-right placeholder) z/right))

(action :paredit/insert-right
        :when z/up :edit #(-> % (z/insert-right placeholder) z/right))

(action :paredit/join-left
        :when #(and (coll-loc? %) (-> % z/left coll-loc?))
        :edit (fn [loc]
                (let [n  (-> loc :path peek dec)
                      cs (-> loc z/left z/node :children)]
                  (-> loc
                      (z/edit (fn [sexp]
                                (update sexp :children #(vec (concat cs %)))))
                      z/up (z/remove-child n) (z/child n)))))

(action :paredit/join-right
        :when #(and (coll-loc? %) (-> % z/right coll-loc?))
        :edit (fn [loc]
                (let [n  (-> loc :path peek)
                      cs (-> loc z/right z/node :children)]
                  (-> loc
                      (z/edit (fn [sexp]
                                (update sexp :children #(vec (concat % cs)))))
                      z/up (z/remove-child (inc n)) (z/child n)))))

(action :paredit/make-curly
        :when coll-loc? :edit #(z/edit % assoc :type :map))

(action :paredit/make-round
        :when coll-loc? :edit #(z/edit % assoc :type :seq))

(action :paredit/make-square
        :when coll-loc? :edit #(z/edit % assoc :type :vec))

(action :paredit/raise
        :when z/up :edit #(-> % z/up (z/replace (z/node %))))

(action :paredit/remove
        :when z/up
        :edit #(if (placeholder-loc? %)
                   (z/remove %)
                   (z/replace % placeholder)))

(action :paredit/shrink-left
        :when #(and (z/up %) (coll-loc? %) (nonempty-loc? %))
        :edit (fn [loc]
                (let [sib (-> loc z/down z/node)]
                  (-> loc (z/remove-child 0) (z/insert-left sib)))))

(action :paredit/shrink-right
        :when #(and (z/up %) (coll-loc? %) (nonempty-loc? %))
        :edit (fn [loc]
                (let [sib (-> loc z/down z/rightmost z/node)]
                  (-> loc (z/edit #(update % :children (comp vec butlast)))
                      (z/insert-right sib)))))

(action :paredit/splice
        :when #(and (z/up %) (coll-loc? %) (nonempty-loc? %))
        :edit (fn [loc]
                (let [n  (-> loc :path peek)
                      cs (-> loc z/node :children)]
                  (-> loc z/up
                      (z/edit #(-> % (assoc-in [:children n] cs)
                                     (update :children (comp vec flatten))))
                      (z/child n)))))

(action :paredit/split-left
        :when #(and (-> % z/up z/up) (not (string-content-loc? %)))
        :edit (fn [loc]
                (let [split (-> loc :path peek)
                      node  (-> loc z/up z/node)
                      [ls rs] (map vec (split-at split (:children node)))]
                  (-> loc z/up
                      (z/edit assoc :children rs)
                      (z/insert-left (assoc node :children ls))
                      z/down))))

(action :paredit/split-right
        :when #(and (-> % z/up z/up) (not (string-content-loc? %)))
        :edit (fn [loc]
                (let [split (-> loc :path peek inc)
                      node  (-> loc z/up z/node)
                      [ls rs] (map vec (split-at split (:children node)))]
                  (-> loc z/up
                      (z/edit assoc :children ls)
                      (z/insert-right (assoc node :children rs))
                      z/down z/rightmost))))

(action :paredit/swap-left
        :when z/left
        :edit (fn [loc]
                (let [n (-> loc :path peek)]
                  (-> loc z/up (z/remove-child n)
                      (z/insert-child (dec n) (z/node loc))
                      (z/child (dec n))))))

(action :paredit/swap-right
        :when z/right
        :edit (fn [loc]
                (let [n (-> loc :path peek)]
                  (-> loc z/up (z/remove-child n)
                      (z/insert-child (inc n) (z/node loc))
                      (z/child (inc n))))))

(defn- wrap [sexp type]
  {:type type :children [sexp]})

(action :paredit/wrap-curly
        :when (complement string-content-loc?)
        :edit #(-> % (z/edit wrap :map) z/down))

(action :paredit/wrap-quote
        :when token-loc?
        :edit (letfn [(wrap-quote [sexp]
                        {:type :string
                         :children [(assoc sexp :type :string-content)]})]
                #(-> % (z/edit wrap-quote) z/down)))

(action :paredit/wrap-round
        :when (complement string-content-loc?)
        :edit #(-> % (z/edit wrap :seq) z/down))

(action :paredit/wrap-square
        :when (complement string-content-loc?)
        :edit #(-> % (z/edit wrap :vec) z/down))
