(ns flense.actions.text
  (:require [flense.model :as m]
            [flense.util :refer [exchange]]
            [xyzzy.core :as z]))

(def ^:private last-caret-pos
  (comp count :text m/unwrap))

(defn- caret-pos [x]
  (let [{:keys [range-start range-end]} (m/unwrap x)]
    (when (= range-start range-end) range-start)))

(defn- move-caret-to [node pos]
  (-> node (assoc :range-start pos :range-end pos) (dissoc :range-direction)))

(defn- move-caret-by [node offset]
  (-> node (move-caret-to (+ (caret-pos node) offset))))

(defn- collapse-range-left [node]
  (-> node (move-caret-to (:range-start node))))

(defn- collapse-range-right [node]
  (-> node (move-caret-to (:range-end node))))

(defn- normalize-range [{start :range-start, end :range-end :as node}]
  (cond-> node (< end start) (exchange :range-start :range-end)
               (caret-pos node) (dissoc :range-direction)))

(def ^:private alphanumeric?
  (partial re-matches #"\w"))

(defn- find-word-boundary-left [x]
  (let [{:keys [range-start text]} (m/unwrap x)]
    (or (->> (range (dec range-start) -1 -1)
             (map (juxt identity #(nth text %)))
             (drop-while (comp (complement alphanumeric?) second))
             (take-while (comp alphanumeric? second))
             last first)
        0)))

(defn- find-word-boundary-right [x]
  (let [{:keys [range-end text]} (m/unwrap x)]
    (or (->> (range range-end (count text))
             (map (juxt identity #(nth text %)))
             (drop-while (comp (complement alphanumeric?) second))
             (drop-while (comp alphanumeric? second))
             ffirst)
        (count text))))

(defn adjust-range-left [loc]
  (when (m/editing? loc)
    (let [node (z/node loc)]
      (cond (= (:range-direction node) :right)
              (-> loc (z/update :range-end dec) (z/edit normalize-range))
            (pos? (:range-start node))
              (-> loc (z/update :range-start dec)
                      (z/assoc :range-direction :left)
                      (z/edit normalize-range))
            :else loc))))

(defn adjust-range-right [loc]
  (when (m/editing? loc)
    (let [node (z/node loc)]
      (cond (= (:range-direction node) :left)
              (-> loc (z/update :range-start inc) (z/edit normalize-range))
            (< (:range-end node) (last-caret-pos node))
              (-> loc (z/update :range-end inc)
                      (z/assoc :range-direction :right)
                      (z/edit normalize-range))
            :else loc))))

(defn adjust-range-left-by-word [loc]
  (when (m/editing? loc)
    (let [node (z/node loc)]
      (if (= (:range-direction node) :right)
        (let [end (-> node collapse-range-right find-word-boundary-left)]
          (if (<= end (:range-start node))
            (z/edit loc collapse-range-left)
            (z/assoc loc :range-end end)))
        (-> loc (z/assoc :range-start (find-word-boundary-left node)
                         :range-direction :left)
                (z/edit normalize-range))))))

(defn adjust-range-right-by-word [loc]
  (when (m/editing? loc)
    (let [node (z/node loc)]
      (if (= (:range-direction node) :left)
        (let [start (-> node collapse-range-left find-word-boundary-right)]
          (if (>= start (:range-end node))
            (z/edit loc collapse-range-right)
            (z/assoc loc :range-start start)))
        (-> loc (z/assoc :range-end (find-word-boundary-right node)
                         :range-direction :right)
                (z/edit normalize-range))))))

(defn begin-editing [loc]
  (when (and (m/stringlike? loc) (not (m/editing? loc)))
    (let [end (last-caret-pos loc)
          start (if (m/placeholder? loc) 0 end)]
      (z/assoc loc :editing? true :range-start start :range-end end
                   :range-direction (when (> end start) :right)))))

(defn cease-editing [loc]
  (when (m/editing? loc)
    (z/dissoc loc :editing? :range-start :range-end :range-direction)))

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

(defn insert [s loc]
  (condp #(%1 %2) loc
    m/atom?
      (if (m/placeholder? loc)
        (z/replace loc (m/string->atom s))
        (z/replace loc (m/string->atom (str (:text (z/node loc)) s))))
    m/editing?
      (if-let [pos (caret-pos loc)]
        (let [{:keys [text]} (z/node loc)
              text' (str (subs text 0 pos) s (subs text pos (count text)))]
          (-> loc (z/assoc :text text') (z/edit move-caret-by (count s))))
        (insert s (delete loc))) ; delete everything in the selected range,
                                 ; then insert at the new caret position
    ;else
      nil))

(defn move-caret-left [loc]
  (when (m/editing? loc)
    (if-let [pos (caret-pos loc)]
      (if (pos? pos)
        (z/edit loc move-caret-by -1)
        (z/edit loc move-caret-to (last-caret-pos loc)))
      (z/edit loc collapse-range-left))))

(defn move-caret-right [loc]
  (when (m/editing? loc)
    (if-let [pos (caret-pos loc)]
      (if (< pos (last-caret-pos loc))
        (z/edit loc move-caret-by 1)
        (z/edit loc move-caret-to 0))
      (z/edit loc collapse-range-right))))

(defn move-caret-left-by-word [loc]
  (when (m/editing? loc)
    (z/edit loc move-caret-to (find-word-boundary-left loc))))

(defn move-caret-right-by-word [loc]
  (when (m/editing? loc)
    (z/edit loc move-caret-to (find-word-boundary-right loc))))

(defn wrap-string [loc]
  (when (m/atom? loc)
    (begin-editing (z/assoc loc :type :string))))
