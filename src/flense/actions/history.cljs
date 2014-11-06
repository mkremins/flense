(ns flense.actions.history)

(defn save [loc prev]
  (-> loc (assoc :prev prev) (dissoc :next)))

(defn redo [{:keys [next] :as loc}]
  (when next (assoc next :prev loc)))

(defn undo [{:keys [prev] :as loc}]
  (when prev (assoc prev :next loc)))
