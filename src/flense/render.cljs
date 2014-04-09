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

(defn- classify [x]
  (cond (false?   x) :bool
        (true?    x) :bool
        (keyword? x) :keyword
        (map?     x) :map
        (nil?     x) :nil
        (number?  x) :number
        (seq?     x) :seq
        (set?     x) :set
        (string?  x) :string
        (symbol?  x) :symbol
        (vector?  x) :vec))

(declare render)

(defn- class-list [{:keys [type selected?]}]
  (str (name type) (when selected? " selected")))

(defn- render-coll [{:keys [children] :as coll}]
  [:div.coll {:class (class-list coll)} children])

(defn- render-token [props]
  (letfn [(do-render [{:keys [text] :as token}]
            [:span.token {:content-editable true
                          :class (class-list token)}
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
        props {:type (classify node) :selected? selected?}]
    (if (coll? node)
        (let [loc (if selected? (-> loc zip/down zip/right) loc)]
          [render-coll (merge props {:children (map render (downs loc))})])
        [render-token (merge props {:text (str node)})])))

(defn root [state]
  (let [curr-loc (zip/edit @state ->SelectedWrapper)]
    [:div.flense (map render (downs (top curr-loc)))]))
