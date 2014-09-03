(ns flense.actions.movement
  (:require [flense.actions :refer [defaction]]
            [flense.model :refer [find-placeholder stringlike-loc?]]
            [xyzzy.core :as z]))

(doseq [[name move]
        {:move/left   z/left-or-wrap
         :move/next   z/next
         :move/next-placeholder #(find-placeholder % z/next)
         :move/prev   z/prev
         :move/prev-placeholder #(find-placeholder % z/prev)
         :move/right  z/right-or-wrap}]
  (defaction name :when move :edit move))

(defaction :move/down
  :when #(if (stringlike-loc? %)
           (not (:editing? (z/node %)))
           (z/down %))
  :edit #(if (stringlike-loc? %)
           (z/edit % assoc :editing? true)
           (z/down %)))

(defaction :move/up
  :when z/up
  :edit #(if (and (stringlike-loc? %) (:editing? (z/node %)))
           (z/edit % dissoc :editing?)
           (z/up %)))
