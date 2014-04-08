(ns flense.render
  (:require [clojure.zip :as zip]))

(defn- right-locs [loc]
  (loop [loc loc locs []]
    (if-let [next-loc (zip/right loc)]
      (recur next-loc (conj locs next-loc))
      locs)))

(defn- downs [loc]
  (if-let [down1 (zip/down loc)]
    (cons down1 (right-locs down1))
    ()))

(defn- top [loc]
  (if-let [up1 (zip/up loc)]
    (recur up1)
    loc))

(declare render)

(defn- render-coll [type children selected?]
  (println (str "render coll of type: " (pr-str type) " with children: " (pr-str children)))
  (let [classes (str (name type) (when selected? " selected"))]
    [:div.coll {:class classes} children]))

(defn- render-token [text selected?]
  (println (str "render token with text: " (pr-str text)))
  [:span.token (if selected? {:class "selected"} {}) text])

(defn- render [loc curr-loc]
  (let [node (zip/node loc)
        selected? (= loc curr-loc)]
    (if (coll? node)
        (render-coll (cond (map? node) :map
                           (seq? node) :seq
                           (set? node) :set
                           (vector? node) :vec)
                     (map #(render % curr-loc) (downs loc))
                     selected?)
        (render-token (if (string? node)
                          (str "\"" node "\"")
                          (str node))
                      selected?))))

(defn root [state]
  (let [curr-loc @state]
    [:div.flense (map #(render % curr-loc) (downs (top curr-loc)))]))
