(ns flense.actions.clipboard
  (:require [flense.model :refer [placeholder placeholder-loc?]]
            [xyzzy.core :as z]))

(def ^{:dynamic true :private true} *clipboard* nil)

(defn- copy [loc]
  (set! *clipboard* (z/node loc))
  loc)

(def actions
  {:clipboard/copy copy
   :clipboard/cut #(-> % copy (z/replace placeholder))
   :clipboard/paste #(when (placeholder-loc? %) (z/replace % *clipboard*))})
