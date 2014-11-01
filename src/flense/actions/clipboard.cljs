(ns flense.actions.clipboard
  (:require [flense.model :refer [placeholder placeholder-loc?]]
            [xyzzy.core :as z]))

(defn- copy [loc]
  (assoc loc :clipboard (z/node loc)))

(def actions
  {:clipboard/copy copy
   :clipboard/cut #(-> % copy (z/replace placeholder))
   :clipboard/paste #(when (placeholder-loc? %) (z/replace % (:clipboard %)))})
