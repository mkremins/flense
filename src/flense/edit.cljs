(ns flense.edit
   (:require [xyzzy.core :as z]))

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
  (let [action (merge {:when (constantly true) :tags #{}}
                      (apply hash-map opts) {:name name})]
    (assert (:edit action))
    (swap! actions assoc name action)))

(def placeholder {:type :symbol :text "..."})

(defn coll-loc? [loc]
  (#{:fn :map :seq :set :vec} (-> loc z/node :type)))

(defn nonempty-loc? [loc]
  (seq (-> loc z/node :children)))

(defn placeholder-loc? [loc]
  (= (-> loc z/node :text) "..."))

(defn string-content-loc? [loc]
  (= (-> loc z/node :type) :string-content))

(defn token-loc? [loc]
  (#{:bool :keyword :nil :number :symbol} (-> loc z/node :type)))
