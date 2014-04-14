(ns flense.render
  (:require [clojure.string :as string]
            [flense.ranges :as ranges]
            [flense.zip :as z]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(defn- coll-node? [{:keys [type]}]
  (#{:map :seq :set :vec} type))

(defn- class-list [{:keys [selected? type] :as node}]
  (string/join " "
    [(name type)
     (if (coll-node? node) "coll" "atom")
     (when selected? "selected")]))

(defn- atom-view [node owner]
  (reify
    om/IRender
    (render [this]
      (dom/span #js {:className (class-list node) :contentEditable true}
        (str (:form node))))

    om/IDidMount
    (did-mount [this]
      (when (:selected? node)
        (doto (om/get-node owner)
          (.focus)
          (ranges/select-contents!))))

    om/IDidUpdate
    (did-update [this prev-props prev-state]
      (if (:selected? node)
          (doto (om/get-node owner)
            (.focus)
            (ranges/select-contents!))
          (when (:selected? prev-props)
            (.blur (om/get-node owner)))))))

(declare node-view)

(defn- coll-view [node owner]
  (reify
    om/IRender
    (render [this]
      (apply dom/div #js {:className (class-list node)}
        (om/build-all node-view (:children node))))))

(defn- node-view [node owner]
  (reify
    om/IRender
    (render [this]
      (om/build
        (if (coll-node? node) coll-view atom-view)
        node))))

(defn root-view [app-state owner]
  (reify
    om/IRender
    (render [this]
      (let [{:keys [path tree]} (:loc app-state)
            tree (update-in tree (z/full-path path) assoc :selected? true)]
        (apply dom/div #js {:className "flense"}
          (om/build-all node-view (:children tree)))))))
