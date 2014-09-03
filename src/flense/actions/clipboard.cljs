(ns flense.actions.clipboard
  (:require [flense.actions :refer [defaction]]
            [flense.model :refer [placeholder placeholder-loc?]]
            [xyzzy.core :as z]))

(def ^{:dynamic true :private true} *clipboard* nil)

(defn- copy [loc]
  (set! *clipboard* (z/node loc))
  loc)

(defaction :clipboard/copy
  :edit copy)

(defaction :clipboard/cut
  :edit #(-> % copy (z/replace placeholder)))

(defaction :clipboard/paste
  :when #(and *clipboard* (placeholder-loc? %))
  :edit #(z/replace % *clipboard*))
