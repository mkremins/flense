(ns flense.edit.movement
  (:require [flense.edit :refer [action placeholder-loc?]]
            [xyzzy.core :as z]))

(defn- find-placeholder [direction loc]
  (let [p-locs (z/find loc placeholder-loc? direction)
        p-loc  (first p-locs)]
    (if (= p-loc loc) (second p-locs) p-loc)))

(doseq [[name move]
        {:move/down   z/down
         :move/left   z/left-or-wrap
         :move/next   z/next
         :move/next-placeholder (partial find-placeholder z/next)
         :move/prev   z/prev
         :move/prev-placeholder (partial find-placeholder z/prev)
         :move/right  z/right-or-wrap
         :move/up     z/up}]
  (action name :when move :edit move))
