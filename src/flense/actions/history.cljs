(ns flense.actions.history)

(def ^:private past-states   (atom []))
(def ^:private future-states (atom []))

(defn push-state! [new-state]
  (reset! future-states [])
  (swap! past-states conj new-state))

(defn redo [_]
  (when (pos? (count @future-states))
    (let [new-state (last @future-states)]
      (swap! past-states conj new-state)
      (swap! future-states pop)
      new-state)))

(defn undo [_]
  (when (> (count @past-states) 1)
    (swap! future-states conj (last @past-states))
    (swap! past-states pop)
    (last @past-states)))

(def actions
  {:history/redo (with-meta redo {:tags #{:history}})
   :history/undo (with-meta undo {:tags #{:history}})})
