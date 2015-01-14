(ns flense.actions.text
  (:require [flense.actions.completions :as completions]
            [flense.model :as m]
            [xyzzy.core :as z]))

(defn begin-editing [loc]
  (when (and (m/stringlike? loc) (not (:editing? (z/node loc))))
    (z/edit loc assoc :editing? true)))

(defn cease-editing [loc]
  (when (m/editing? loc)
    (z/edit loc dissoc :editing?)))

(defn delete-char [loc]
  (condp #(%1 %2) loc
    (every-pred m/atom? (complement m/placeholder?))
      (let [{:keys [text]} (z/node loc)]
        (if (> (count text) 1)
          (z/replace loc (m/string->atom (subs text 0 (dec (count text)))))
          (z/replace loc m/placeholder)))
    (every-pred m/editing? (comp seq :text z/node))
      (z/edit loc update :text #(subs % 0 (dec (count %))))
    ;else
      nil))

(defn insert-char [c loc]
  (condp #(%1 %2) loc
    m/atom?
      (if (m/placeholder? loc)
        (z/replace loc (m/string->atom c))
        (z/replace loc (m/string->atom (str (:text (z/node loc)) c))))
    m/editing?
      (if (m/placeholder? loc)
        (z/edit loc assoc :text c)
        (z/edit loc update :text str c))
    ;else
      nil))

(defn wrap-string [loc]
  (when (m/atom? loc)
    (z/edit loc assoc :type :string :editing? true)))
