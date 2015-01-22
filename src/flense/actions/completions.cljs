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
      (cond-> (z/replace loc (m/form->tree form))
              (= type :template) m/next-placeholder))))

(defn next-completion [loc]
  (when (has-completions? loc)
    (let [{:keys [completions selected-completion]} (z/node loc)]
      (if (< selected-completion (dec (count completions)))
        (z/update loc :selected-completion inc)
        (z/assoc loc :selected-completion 0)))))

(defn prev-completion [loc]
  (when (and (has-completions? loc) (pos? (:selected-completion (z/node loc))))
    (z/update loc :selected-completion dec)))

(defn local-names [loc]
  (->> (clj/collect-binding-locs loc)
       (map (comp :text z/node)) (remove #{"..."}) distinct))

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
  "Naïve string similarity algorithm. Returns the length of the longest prefix
  shared by `s1` and `s2`. Should probably be replaced by something better at
  some point in the future."
  [s1 s2]
  (cond (= s1 s2) (count s1)
        (= (first s1) (first s2)) (inc (similarity (rest s1) (rest s2)))
        :else 0))

(defn completions
  "Given a location `loc`, returns a seq of completions that might be inserted
  at that location. Each completion is a tuple `[type form]`, where `type` is a
  keyword naming the type of thing to insert and `form` is a Clojure form."
  [loc]
  (when (m/atom? loc)
    (if (m/placeholder? loc)
      (concat (map #(-> [:local (symbol %)]) (take 3 (local-names loc)))
              (map #(-> [:core %]) '[fn if let loop when]))
      (let [text (:text (z/node loc))
            best-of #(->> (map (juxt identity (partial similarity text)) %)
                          (filter (comp pos? second)) (sort-by second >)
                          (map first) (take 3))]
        (concat (when-let [template (templates text)] [[:template template]])
                (map #(-> [:local (symbol %)]) (best-of (local-names loc)))
                (map #(-> [:core (symbol %)]) (best-of (keys templates))))))))

(defn update-completions [loc]
  (if (m/atom? loc)
    (let [completions (completions loc)
          selected (:selected-completion (z/node loc))
          idx (if (< selected (count completions)) selected 0)]
      (z/assoc loc :completions completions :selected-completion idx))
    loc))
