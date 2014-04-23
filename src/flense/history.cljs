(ns flense.history)

(def ^:private past-states   (atom []))
(def ^:private future-states (atom []))

(defn- can-undo? []
  (> (count @past-states) 1))

(defn- can-redo? []
  (pos? (count @future-states)))

(defn undo [app-state]
  (if (can-undo?)
      (do (swap! future-states conj (last @past-states))
          (swap! past-states pop)
          (last @past-states))
      app-state))

(defn redo [app-state]
  (if (can-redo?)
      (let [new-state (last @future-states)]
        (swap! past-states conj new-state)
        (swap! future-states pop)
        new-state)
      app-state))

(defn push-state! [new-state]
  (reset! future-states [])
  (swap! past-states conj new-state))
