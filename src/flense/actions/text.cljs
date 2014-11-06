(ns flense.actions.text
  (:require [flense.model :as m]
            [xyzzy.core :as z]))

(defn delete-char [loc]
  (when (and (m/atom-loc? loc) (not (m/placeholder-loc? loc)))
    (let [{:keys [text]} (z/node loc)]
      (if (> (count text) 1)
        (z/replace loc (m/string->atom (subs text 0 (dec (count text)))))
        (z/replace loc m/placeholder)))))

(defn insert-char [c loc]
  (when (m/atom-loc? loc)
    (if (m/placeholder-loc? loc)
      (z/replace loc (m/string->atom c))
      (z/replace loc (m/string->atom (str (:text (z/node loc)) c))))))

(defn down [loc]
  (if (and (m/stringlike-loc? loc) (not (:editing? (z/node loc))))
    (z/edit loc assoc :editing? true)
    (z/down loc)))

(defn up [loc]
  (if (and (m/stringlike-loc? loc) (:editing? (z/node loc)))
    (z/edit loc dissoc :editing?)
    (z/up loc)))

(defn wrap-string [loc]
  (when (m/atom-loc? loc)
    (z/edit loc assoc :type :string :editing? true)))
