(ns flense.actions.text
  (:require [flense.actions.completions :as completions]
            [flense.model :as m]
            [xyzzy.core :as z]))

(defn delete-char [loc]
  (when (and (m/atom? loc) (not (m/placeholder? loc)))
    (let [{:keys [text]} (z/node loc)]
      (if (> (count text) 1)
        (z/replace loc (m/string->atom (subs text 0 (dec (count text)))))
        (z/replace loc m/placeholder)))))

(defn insert-char [c loc]
  (when (m/atom? loc)
    (if (m/placeholder? loc)
      (z/replace loc (m/string->atom c))
      (z/replace loc (m/string->atom (str (:text (z/node loc)) c))))))

(defn down [loc]
  (if (and (m/stringlike? loc) (not (:editing? (z/node loc))))
    (z/edit loc assoc :editing? true)
    (or (completions/next-completion loc) (z/down loc))))

(defn up [loc]
  (if (and (m/stringlike? loc) (:editing? (z/node loc)))
    (z/edit loc dissoc :editing?)
    (z/up loc)))

(defn wrap-string [loc]
  (when (m/atom? loc)
    (z/edit loc assoc :type :string :editing? true)))
