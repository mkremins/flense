(ns flense.actions.completions
  (:require [flense.actions.clojure :as clj]
            [flense.model :as m]
            [flense.util :refer [update]]
            [xyzzy.core :as z]))

(defn complete [loc]
  (when (and (m/atom? loc) (seq (:completions (z/node loc))))
    (let [{:keys [completions selected-completion]} (z/node loc)
          [type form] (nth completions (or selected-completion 0))]
      (cond-> loc :always (z/replace (m/form->tree form))
                  (= type :template) m/next-placeholder))))

(defn next-completion [loc]
  (when (and (m/atom? loc) (seq (:completions (z/node loc))))
    (let [{:keys [completions selected-completion]} (z/node loc)]
      (if (< selected-completion (dec (count completions)))
        (z/edit loc update :selected-completion inc)
        (z/edit loc assoc :selected-completion 0)))))

(defn locals [loc]
  (distinct (map (comp symbol :text z/node) (clj/collect-binding-locs loc))))

(def templates
  '{"def"   (def ... ...)
    "defn"  (defn ... [...] ...)
    "defn-" (defn- ... [...] ...)
    "fn"    (fn [...] ...)
    "if"    (if ... ... ...)
    "let"   (let [... ...] ...)
    "when"  (when ... ...)})

(defn completions [loc]
  (when (m/atom? loc)
    (concat (when-let [template (templates (:text (z/node loc)))]
              [[:template template]])
            (map #(-> [:local %]) (locals loc)))))

(defn update-completions [loc]
  (if (m/atom? loc)
    (let [completions (completions loc)
          selected (:selected-completion (z/node loc))
          idx (if (< selected (count completions)) selected 0)]
      (z/edit loc assoc :completions completions :selected-completion idx))
    loc))
