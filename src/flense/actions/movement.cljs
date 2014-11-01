(ns flense.actions.movement
  (:require [flense.model :refer [find-placeholder stringlike-loc?]]
            [xyzzy.core :as z]))

(defn down [loc]
  (if (stringlike-loc? loc)
    (when-not (:editing? (z/node loc))
      (z/edit loc assoc :editing? true))
    (z/down loc)))

(defn up [loc]
  (if (and (stringlike-loc? loc) (:editing? (z/node loc)))
    (z/edit loc dissoc :editing?)
    (z/up loc)))

(def actions
  {:move/down down
   :move/up up
   :move/left z/left-or-wrap
   :move/right z/right-or-wrap
   :move/next z/next
   :move/prev z/prev
   :move/next-placeholder #(find-placeholder % z/next)
   :move/prev-placeholder #(find-placeholder % z/prev)})
