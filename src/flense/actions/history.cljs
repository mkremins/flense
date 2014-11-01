(ns flense.actions.history)

(defn redo [{:keys [next] :as loc}]
  (when next (assoc next :prev loc)))

(defn undo [{:keys [prev] :as loc}]
  (when prev (assoc prev :next loc)))

(def actions
  {:history/redo (with-meta redo {:tags #{:history}})
   :history/undo (with-meta undo {:tags #{:history}})})
