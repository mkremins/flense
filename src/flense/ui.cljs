(ns flense.ui
  (:require [cljs.core.async :as async]
            [clojure.string :as string]
            [flense.edit :as e]
            [flense.keyboard :refer [key-data]]
            [flense.parse :as p]
            [flense.zip :as z]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true])
  (:require-macros [cljs.core.async.macros :refer [go-loop]]))

(defn- class-list [{:keys [selected? type] :as node}]
  (string/join " "
   [(name type)
    (if (p/coll-node? node) "coll" "atom")
    (when selected? "selected")]))

(defn- fully-selected? [input]
  (and (= (.-selectionStart input) 0)
       (= (.-selectionEnd input) (count (.-value input)))))

(def ^:private max-chars 60)

(defn- line-count [text]
  (inc (int (/ (count text) (- max-chars 2)))))

(defn- px [n]
  (str n "px"))

(defn- render-width [content]
  (let [tester (.getElementById js/document "width-tester")]
    (set! (.-textContent tester) content)
    (inc (.-clientWidth tester))))

(def ^:private char-width
  (render-width "_"))

(def ^:private max-width
  (render-width (string/join (repeat max-chars " "))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; atom views
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- handle-key [ev data]
  (when (and (= (key-data ev) #{:BKSPACE})
             (not (fully-selected? (.-target ev))))
    (.stopPropagation ev)))

(defn- atom-view [node owner]
  (reify
    om/IRender
    (render [_]
      (dom/input
        #js {:className (class-list node)
             :onChange  #(om/update! node (p/parse-atom (.. % -target -value)))
             :onKeyDown #(handle-key % node)
             :style #js {:width (px (render-width (p/tree->str node)))}
             :value (pr-str (:form node))}))

    om/IDidMount
    (did-mount [_]
      (when (:selected? node)
        (doto (om/get-node owner)
          (.focus)
          (.select))))

    om/IDidUpdate
    (did-update [_ prev-props prev-state]
      (if (:selected? node)
          (when (or (not (:selected? prev-props)) (= (:form node) '...))
            (doto (om/get-node owner)
              (.focus)
              (.select)))
          (when (:selected? prev-props)
            (.blur (om/get-node owner)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; string content views
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- handle-string-key [ev data]
  (when (#{#{:BKSPACE}
           #{:LBRAK}
           #{:SPACE}
           #{:SHIFT :LBRAK}
           #{:SHIFT :NINE}
           #{:SHIFT :QUOTE}} (key-data ev))
    (.stopPropagation ev)))

(defn- string-content-view [node owner]
  (reify
    om/IRender
    (render [_]
      (let [text (string/replace (:text node) #"\s+" " ")]
        (dom/textarea
          #js {:className (class-list node)
               :onChange  #(om/update! node :text (.. % -target -value))
               :onKeyDown #(handle-string-key % node)
               :style #js {:height (str (* 1.3 (line-count text)) "rem")
                           :width  (px (min (render-width text) max-width))}
               :value text})))

    om/IDidMount
    (did-mount [_]
      (when (:selected? node)
        (doto (om/get-node owner)
          (.focus)
          (.select))))

    om/IDidUpdate
    (did-update [_ prev-props prev-state]
      (if (:selected? node)
          (when (or (not (:selected? prev-props)) (= (:text node) "..."))
            (doto (om/get-node owner)
              (.focus)
              (.select)))
          (when (:selected? prev-props)
            (.blur (om/get-node owner)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; collection, generic, root views
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare node-view)

(defn- head-size [items]
  (let [itemc (count items)]
    (loop [offset char-width
           idxs (range itemc)]
      (if-let [idx (first idxs)]
        (let [item-width (render-width (p/tree->str (nth items idx)))
              offset' (+ offset char-width item-width)]
          (if (> offset' max-width) idx (recur offset' (rest idxs))))
        itemc))))

(defn- seq-view [data owner]
  (reify om/IRender
    (render [_]
      (let [children (:children data)
            headc (head-size children)]
        (apply dom/div #js {:className (class-list data)}
          (concat (om/build-all node-view (take headc children))
                  [(apply dom/div #js {:className "runoff-children"}
                    (om/build-all node-view (drop headc children)))]))))))

(defn- coll-view [node owner]
  (reify om/IRender
    (render [_]
      (apply dom/div #js {:className (class-list node)}
        (om/build-all node-view (:children node))))))

(defn- node-view [node owner]
  (reify
    om/IRender
    (render [this]
      (om/build
        (cond
          (= (:type node) :seq) seq-view
          (p/coll-node? node) coll-view
          (= (:type node) :string-content) string-content-view
          :else atom-view)
        node))))

(defn root-view [app-state owner]
  (reify
    om/IWillMount
    (will-mount [_]
      (let [tx-chan (om/get-shared owner :tx-chan)]
        (go-loop []
          (let [tx (<! tx-chan)]
            (om/transact! app-state (or (:path tx) []) (:fn tx) (:tag tx)))
          (recur))))
    om/IRender
    (render [_]
      (let [{:keys [tree]} (z/edit app-state assoc :selected? true)]
        (apply dom/div #js {:className "flense"}
          (om/build-all node-view (:children tree)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; command bar view
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- handle-command-bar-key [owner ev]
  (condp = (key-data ev)
    #{:ENTER} (let [input (.-target ev)]
                (async/put!
                 (om/get-shared owner :command-chan)
                 (string/split (.-value input) #"\s+"))
                (set! (.-value input) "")
                (.blur input))
    #{:ESC} (.. ev -target blur) nil)
  (.stopPropagation ev)) ; allow default behavior instead of keybound

(defn command-bar-view [app-state owner]
  (reify om/IRender
    (render [_]
      (dom/input
        #js {:id "command-bar"
             :onKeyDown (partial handle-command-bar-key owner)}))))
