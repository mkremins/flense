(ns flense.actions.text
  (:require [flense.actions.completions :as completions]
            [flense.model :as m]
            [xyzzy.core :as z]))

(def ^:private last-caret-pos
  (comp count :text m/unwrap))

(defn begin-editing [loc]
  (when (and (m/stringlike? loc) (not (m/editing? loc)))
    (z/assoc loc :editing? true :caret-pos (last-caret-pos loc))))

(defn cease-editing [loc]
  (when (m/editing? loc)
    (z/dissoc loc :editing?)))

(defn next-char [loc]
  (when (m/editing? loc)
    (let [node (z/node loc)]
      (if (< (:caret-pos node) (last-caret-pos node))
        (z/update loc :caret-pos inc)
        (z/assoc loc :caret-pos 0)))))

(defn prev-char [loc]
  (when (m/editing? loc)
    (let [node (z/node loc)]
      (if (pos? (:caret-pos node))
        (z/update loc :caret-pos dec)
        (z/assoc loc :caret-pos (last-caret-pos node))))))

(defn delete-char [loc]
  (condp #(%1 %2) loc
    (every-pred m/atom? (complement m/placeholder?))
      (let [{:keys [text]} (z/node loc)]
        (if (> (count text) 1)
          (z/replace loc (m/string->atom (subs text 0 (dec (count text)))))
          (z/replace loc m/placeholder)))
    (every-pred m/editing? (comp seq :text z/node))
      (let [{i :caret-pos, :keys [text]} (z/node loc)
            text' (str (subs text 0 (dec i)) (subs text i (count text)))]
        (-> loc (z/assoc :text text') (z/update :caret-pos #(max (dec %) 0))))
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
        (z/assoc loc :text c :caret-pos 1)
        (let [{i :caret-pos, :keys [text]} (z/node loc)
              text' (str (subs text 0 i) c (subs text i (count text)))]
          (-> loc (z/assoc :text text') (z/update :caret-pos inc))))
    ;else
      nil))

(defn wrap-string [loc]
  (when (m/atom? loc)
    (begin-editing (z/assoc loc :type :string))))
