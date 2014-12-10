(ns flense.actions.completions
  (:require [flense.actions.clojure :as clj]
            [flense.model :as m]
            [xyzzy.core :as z]))

(defn has-completions? [x]
  (and (m/atom? x) (seq (:completions (m/unwrap x)))))

(defn complete [loc]
  (when (has-completions? loc)
    (let [{:keys [completions selected-completion]} (z/node loc)
          [type form] (nth completions (or selected-completion 0))]
      (cond-> loc :always (z/replace (m/form->tree form))
                  (= type :template) m/next-placeholder))))

(defn next-completion [loc]
  (when (has-completions? loc)
    (let [{:keys [completions selected-completion]} (z/node loc)]
      (if (< selected-completion (dec (count completions)))
        (z/edit loc update :selected-completion inc)
        (z/edit loc assoc :selected-completion 0)))))

(defn prev-completion [loc]
  (when (has-completions? loc)
    (let [{:keys [completions selected-completion]} (z/node loc)]
      (if (pos? selected-completion)
        (z/edit loc update :selected-completion dec)
        (z/edit loc assoc :selected-completion (dec (count completions)))))))

(defn local-names [loc]
  (distinct (map (comp :text z/node) (clj/collect-binding-locs loc))))

(def templates
  '{"def"       (def ... ...)
    "defmacro"  (defmacro ... [...] ...)
    "defn"      (defn ... [...] ...)
    "defn-"     (defn- ... [...] ...)
    "fn"        (fn [...] ...)
    "if"        (if ... ... ...)
    "if-let"    (if-let [... ...] ... ...)
    "let"       (let [... ...] ...)
    "loop"      (loop [... ...] ...)
    "when"      (when ... ...)
    "when-let"  (when-let [... ...] ...)})

(defn similarity
  "Given two strings `s1` and `s2`, returns a non-negative number that is
  greater when the strings are more similar. Note that this is an ad-hoc string
  similarity algorithm I made up on the spot, not a well-documented solution;
  at some point it should probably be swapped out for something better."
  [s1 s2]
  (let [freqs1 (frequencies s1)
        freqs2 (frequencies s2)]
    (/ (reduce (fn [score c] (+ score (min (freqs1 c) (freqs2 c))))
               0 (distinct (concat (keys freqs1) (keys freqs2))))
       (inc (js/Math.abs (- (count s1) (count s2)))))))

(defn completions
  "Given a location `loc`, returns a seq of completions that might be inserted
  at that location. Each completion is a tuple `[type form]`, where `type` is a
  keyword naming the type of thing to insert and `form` is a Clojure form."
  [loc]
  (when (m/atom? loc)
    (let [text (:text (z/node loc))
          best-of #(->> (map (juxt identity (partial similarity text)) %)
                        (filter (comp pos? second)) (sort-by second >)
                        (map first) (take 3))]
      (concat (when-let [template (templates text)] [[:template template]])
              (map #(-> [:local (symbol %)]) (best-of (local-names loc)))
              (map #(-> [:core (symbol %)]) (best-of (keys templates)))))))

(defn update-completions [loc]
  (if (m/atom? loc)
    (let [completions (completions loc)
          selected (:selected-completion (z/node loc))
          idx (if (< selected (count completions)) selected 0)]
      (z/edit loc assoc :completions completions :selected-completion idx))
    loc))
