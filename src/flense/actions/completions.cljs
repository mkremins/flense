(ns flense.actions.completions
  (:require [flense.actions.clojure :as clj]
            [flense.model :as m]
            [flense.util :refer [update]]
            [xyzzy.core :as z]))

(defn complete [loc]
  (when (and (m/atom? loc) (seq (:completions (z/node loc))))
    (let [{:keys [completions selected-completion]} (z/node loc)
          selected-completion (or selected-completion 0)]
      (z/replace loc (m/string->atom (nth completions selected-completion))))))

(defn next-completion [loc]
  (when (and (m/atom? loc) (seq (:completions (z/node loc))))
    (let [{:keys [completions selected-completion]} (z/node loc)]
      (if (< selected-completion (dec (count completions)))
        (z/edit loc update :selected-completion inc)
        (z/edit loc assoc :selected-completion 0)))))

(defn update-completions [loc]
  (if (m/atom? loc)
    (let [sym-names (map (comp :text z/node) (clj/collect-binding-locs loc))
          selected (:selected-completion (z/node loc))
          idx (if (< selected (count sym-names)) selected 0)]
      (z/edit loc assoc :completions sym-names :selected-completion idx))
    loc))
