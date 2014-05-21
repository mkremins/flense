(ns flense.edit.clipboard
  (:require [flense.edit :refer [action placeholder placeholder-loc?]]
            [xyzzy.core :as z]))

(def ^{:dynamic true :private true} *clipboard* nil)

(defn- copy [loc]
  (set! *clipboard* (z/node loc))
  loc)

(action :clipboard/copy
        :edit copy)

(action :clipboard/cut
        :edit #(-> % copy (z/replace placeholder)))

(action :clipboard/paste
        :when #(and *clipboard* (placeholder-loc? %))
        :edit #(z/replace % *clipboard*))
