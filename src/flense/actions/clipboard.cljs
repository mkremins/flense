(ns flense.actions.clipboard
  (:require [flense.model :as model]
            [xyzzy.core :as z]))

(defn copy [loc]
  (assoc loc :clipboard (z/node loc)))

(defn cut [loc]
  (-> loc copy (z/replace model/placeholder)))

(defn paste [loc]
  (when (model/placeholder? loc)
    (z/replace loc (:clipboard loc))))
