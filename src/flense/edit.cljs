(ns flense.edit
   (:require [flense.util :refer [update]]
             [xyzzy.core :as z]))

(def actions (atom {}))

(defn action
  "Defines an action with name `name` and options `opts`, where `name` is a
   keyword naming the action and `opts` are kwargs as specified below.

   Required options:

     :edit => Function that, when applied to a loc for which `:when` returns
              truthy, returns a modified loc to use as the new document.

   Permitted options:

     :when => Function that, when applied to a loc, returns truthy if the
              action can be performed there. Defaults to `(constantly true)`.
     :tags => Set of keywords labeling the action in the edit stream. Defaults
              to the empty set.
  "
  [name & opts]
  (let [{:keys [edit tags], pred :when
         :or {pred (constantly true), tags #{}}} (apply hash-map opts)
        action {:name name :pred pred :edit edit :tags tags}]
    (assert edit)
    (swap! actions assoc name action)))
