(ns flense.actions.text
  (:require [flense.actions.completions :as completions]
            [flense.model :as m]
            [xyzzy.core :as z]))

(defn- last-char-idx [x]
  (-> (m/unwrap x) :text count dec))

(defn begin-editing [loc]
  (when (and (m/stringlike? loc) (not (m/editing? loc)))
    (z/assoc loc :editing? true :char-idx (last-char-idx loc))))

(defn cease-editing [loc]
  (when (m/editing? loc)
    (z/dissoc loc :editing?)))

(defn next-char [loc]
  (when (m/editing? loc)
    (let [node (z/node loc)]
      (if (< (:char-idx node) (last-char-idx node))
        (z/update loc :char-idx inc)
        (z/assoc loc :char-idx 0)))))

(defn prev-char [loc]
  (when (m/editing? loc)
    (let [node (z/node loc)]
      (if (pos? (:char-idx node))
        (z/update loc :char-idx dec)
        (z/assoc loc :char-idx (last-char-idx node))))))

(defn delete-char [loc]
  (condp #(%1 %2) loc
    (every-pred m/atom? (complement m/placeholder?))
      (let [{:keys [text]} (z/node loc)]
        (if (> (count text) 1)
          (z/replace loc (m/string->atom (subs text 0 (dec (count text)))))
          (z/replace loc m/placeholder)))
    (every-pred m/editing? (comp seq :text z/node))
      (let [{i :char-idx, :keys [text]} (z/node loc)
            text' (str (subs text 0 i) (subs text (inc i) (count text)))]
        (-> loc (z/assoc :text text') (z/update :char-idx dec)))
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
        (z/assoc loc :text c :char-idx 0)
        (let [{i :char-idx, :keys [text]} (z/node loc)
              text' (str (subs text 0 (inc i)) c
                         (subs text (inc i) (count text)))]
          (-> loc (z/assoc :text text') (z/update :char-idx inc))))
    ;else
      nil))

(defn wrap-string [loc]
  (when (m/atom? loc)
    (begin-editing (z/assoc loc :type :string))))
