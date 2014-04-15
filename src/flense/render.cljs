(ns flense.render
  (:require [clojure.string :as string]
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

(defn- parse-symbol-or-number [text]
  (let [number (js/parseFloat text)]
    (if (js/isNaN number)
        {:type :symbol :form (symbol text)}
        {:type :number :form number})))

(defn- parse-atom [text]
  (cond
    (= text "false") {:type :bool :form false}
    (= text "nil")   {:type :nil  :form nil}
    (= text "true")  {:type :bool :form true}
    :else            (parse-symbol-or-number text)))

(defn- atom-width [form]
  (let [tester (.getElementById js/document "width-tester")]
    (set! (.-textContent tester) (pr-str form))
    (str (inc (.-clientWidth tester)) "px")))

(defn- atom-view [node owner]
  (reify
    om/IRender
    (render [this]
      (dom/input
        #js {:className (class-list node)
             :onChange #(om/update! node (parse-atom (.. % -target -value)))
             :style #js {:width (atom-width (:form node))}
             :value (pr-str (:form node))}))

    om/IDidMount
    (did-mount [this]
      (when (:selected? node)
        (doto (om/get-node owner)
          (.focus)
          (.select))))

    om/IDidUpdate
    (did-update [this prev-props prev-state]
      (if (:selected? node)
          (when-not (:selected? prev-props)
            (doto (om/get-node owner)
              (.focus)
              (.select)))
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
