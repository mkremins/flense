(ns flense.zip.util
  (:require [flense.util :refer [delete insert lconj update]]))

(defn delete-child [parent i]
  (update parent :children delete i))

(defn delete-leftmost [parent]
  (update parent :children delete 0))

(defn delete-rightmost [parent]
  (update parent :children #(if (empty? %) % (pop %))))

(defn insert-child [parent i child]
  (update parent :children insert i child))

(defn insert-leftmost [parent _ child]
  (update parent :children lconj child))

(defn insert-rightmost [parent _ child]
  (update parent :children conj child))
