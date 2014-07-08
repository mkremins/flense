(ns flense.edit.movement
  (:require [flense.edit :refer [action placeholder-loc? stringlike-loc?]]
            [xyzzy.core :as z]))

(defn- find-placeholder [direction loc]
  (z/find-next loc placeholder-loc? direction))

(doseq [[name move]
        {:move/left   z/left-or-wrap
         :move/next   z/next
         :move/next-placeholder (partial find-placeholder z/next)
         :move/prev   z/prev
         :move/prev-placeholder (partial find-placeholder z/prev)
         :move/right  z/right-or-wrap}]
  (action name :when move :edit move))

(action :move/down
        :when #(if (stringlike-loc? %)
                 (not (:editing? (z/node %)))
                 (z/down %))
        :edit #(if (stringlike-loc? %)
                 (z/edit % assoc :editing? true)
                 (z/down %)))

(action :move/up
        :when z/up
        :edit #(if (and (stringlike-loc? %) (:editing? (z/node %)))
                 (z/edit % dissoc :editing?)
                 (z/up %)))
