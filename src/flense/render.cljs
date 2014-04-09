(ns flense.render
  (:require [clojure.zip :as zip]
            [flense.ranges :as ranges]
            [reagent.core :as reagent]))

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

(defn- render-coll [{:keys [type children selected?]}]
  (let [classes (str (name type) (when selected? " selected"))]
    [:div.coll {:class classes} children]))

(defn- render-token [props]
  (letfn [(do-render [{:keys [text selected?]}]
            [:span.token (merge {:content-editable true}
                                (when selected? {:class "selected"}))
                         text])]
    (with-meta do-render
               {:component-did-update
                (fn [this]
                  (let [dom-node (reagent/dom-node this)]
                    (when (.contains (.-classList dom-node) "selected")
                      (.focus dom-node)
                      (ranges/select-contents! dom-node))))})))

(defrecord SelectedWrapper [node])

(defn- render [loc]
  (let [node (zip/node loc)
        selected? (instance? SelectedWrapper node)
        node (if selected? (:node node) node)
        parent (if selected? (-> loc zip/down zip/right) loc)]
    (if (coll? node)
        [render-coll {:type (cond (map? node) :map
                                  (seq? node) :seq
                                  (set? node) :set
                                  (vector? node) :vec)
                      :children (map render (downs parent))
                      :selected? selected?}]
        [render-token {:text (if (string? node)
                                 (str "\"" node "\"")
                                 (str node))
                       :selected? selected?}])))

(defn root [state]
  (let [curr-loc (zip/edit @state ->SelectedWrapper)]
    [:div.flense (map render (downs (top curr-loc)))]))
