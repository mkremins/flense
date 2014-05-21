(ns flense.edit.history
  (:require [flense.edit :refer [action]]))

(def ^:private past-states   (atom []))
(def ^:private future-states (atom []))

(defn push-state! [new-state]
  (reset! future-states [])
  (swap! past-states conj new-state))

(action :history/undo
        :when #(> (count @past-states) 1)
        :edit #(do (swap! future-states conj (last @past-states))
                   (swap! past-states pop)
                   (last @past-states))
        :tags #{:history})

(action :history/redo
        :when #(pos? (count @future-states))
        :edit #(let [new-state (last @future-states)]
                 (swap! past-states conj new-state)
                 (swap! future-states pop)
                 new-state)
        :tags #{:history})
