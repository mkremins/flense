(ns flense.actions.text
  (:require [flense.actions.completions :as completions]
            [flense.model :as m]
            [xyzzy.core :as z]))

(def ^:private last-caret-pos
  (comp count :text m/unwrap))

(defn- caret-pos [x]
  (let [{:keys [range-start range-end]} (m/unwrap x)]
    (when (= range-start range-end) range-start)))

(defn- move-caret-to [node pos]
  (assoc node :range-start pos :range-end pos))

(defn- move-caret-by [node f]
  (-> node (update :range-start f) (update :range-end f)))

(defn begin-editing [loc]
  (when (and (m/stringlike? loc) (not (m/editing? loc)))
    (let [end (last-caret-pos loc)
          start (if (m/placeholder? loc) 0 end)]
      (z/assoc loc :editing? true :range-start start :range-end end))))

(defn cease-editing [loc]
  (when (m/editing? loc)
    (z/dissoc loc :editing? :range-start :range-end)))

(defn next-char [loc]
  (when (m/editing? loc)
    (if-let [pos (caret-pos loc)]
      (if (< pos (last-caret-pos loc))
        (z/edit loc move-caret-by inc)
        (z/edit loc move-caret-to 0))
      loc)))

(defn prev-char [loc]
  (when (m/editing? loc)
    (if-let [pos (caret-pos loc)]
      (if (pos? pos)
        (z/edit loc move-caret-by dec)
        (z/edit loc move-caret-to (last-caret-pos loc)))
      loc)))

(defn delete [loc]
  (condp #(%1 %2) loc
    (every-pred m/atom? (complement m/placeholder?))
      (let [{:keys [text]} (z/node loc)]
        (if (> (count text) 1)
          (z/replace loc (m/string->atom (subs text 0 (dec (count text)))))
          (z/replace loc m/placeholder)))
    (every-pred m/editing? (comp seq :text z/node))
      (let [{start :range-start, end :range-end, :keys [text]} (z/node loc)
            pos (caret-pos loc)
            text' (if pos
                    (str (subs text 0 (dec pos)) (subs text pos (count text)))
                    (str (subs text 0 start) (subs text end (count text))))]
        (-> loc (z/assoc :text text')
                (z/edit move-caret-to (if pos (max (dec pos) 0) start))))
    ;else
      nil))

(defn insert [c loc]
  (condp #(%1 %2) loc
    m/atom?
      (if (m/placeholder? loc)
        (z/replace loc (m/string->atom c))
        (z/replace loc (m/string->atom (str (:text (z/node loc)) c))))
    m/editing?
      (if-let [pos (caret-pos loc)]
        (let [{:keys [text]} (z/node loc)
              text' (str (subs text 0 pos) c (subs text pos (count text)))]
          (-> loc (z/assoc :text text') (z/edit move-caret-by inc)))
        (insert c (delete loc)))
    ;else
      nil))

(defn wrap-string [loc]
  (when (m/atom? loc)
    (begin-editing (z/assoc loc :type :string))))
