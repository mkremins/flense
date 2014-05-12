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

(defn- class-list [{:keys [selected? type] :as data}]
  (string/join " "
   [(name type)
    (if (p/coll-node? data) "coll" "token")
    (when selected? "selected")]))

(defn- move-caret-to-end [input]
  (let [idx (count (.-value input))]
    (set! (.-selectionStart input) idx)
    (set! (.-selectionEnd input) idx)))

(defn- fully-selected? [input]
  (and (= (.-selectionStart input) 0)
       (= (.-selectionEnd input) (count (.-value input)))))

(def ^:private MAX_CHARS 72)

(defn- line-count [text]
  (inc (int (/ (count text) (- MAX_CHARS 2)))))

(defn- px [n]
  (str n "px"))

(defn- render-width [content]
  (let [tester (.getElementById js/document "width-tester")]
    (set! (.-textContent tester) content)
    (inc (.-clientWidth tester))))

(def ^:private MAX_WIDTH
  (render-width (string/join (repeat MAX_CHARS "_"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; token views
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- handle-key [ev]
  (when (and (= (key-data ev) #{:BACKSPACE})
             (not (fully-selected? (.-target ev))))
    (.stopPropagation ev)))

(defn- token-view [data owner]
  (reify
    om/IRender
    (render [_]
      (dom/input
        #js {:className (class-list data)
             :onChange #(om/update! data (p/parse-token (.. % -target -value)))
             :onKeyDown handle-key
             :style #js {:width (px (render-width (p/tree->str data)))}
             :value (:text data)}))
    om/IDidMount
    (did-mount [_]
      (when (:selected? data) (doto (om/get-node owner) .focus .select)))
    om/IDidUpdate
    (did-update [_ prev _]
      (let [input (om/get-node owner)]
        (if (:selected? data)
            (when (or (not (:selected? prev)) (p/placeholder-node? data))
              (doto input .focus .select))
            (when (:selected? prev) (.blur input)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; string content views
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- handle-string-key [ev]
  (when (#{#{:BACKSPACE}
           #{:LEFT}
           #{:OPEN_BRACKET}
           #{:RIGHT}
           #{:SPACE}
           #{:SHIFT :NINE}
           #{:SHIFT :OPEN_BRACKET}
           #{:SHIFT :QUOTE}
           #{:SHIFT :THREE}} (key-data ev))
    (.stopPropagation ev)))

(defn- string-content-view [data owner]
  (reify
    om/IRender
    (render [_]
      (let [text (string/replace (:text data) #"\s+" " ")]
        (dom/textarea
          #js {:className (class-list data)
               :onChange #(om/update! data :text (.. % -target -value))
               :onKeyDown handle-string-key
               :style #js {:height (str (* 1.2 (line-count text)) "rem")
                           :width  (px (min (render-width text) MAX_WIDTH))}
               :value text})))
    om/IDidMount
    (did-mount [_]
      (when (:selected? data)
        (let [input (om/get-node owner)]
          (if (p/placeholder-node? data)
              (doto input .focus .select)
              (move-caret-to-end input)))))
    om/IDidUpdate
    (did-update [_ prev _]
      (let [input (om/get-node owner)]
        (if (:selected? data)
            (when-not (:selected? prev) (move-caret-to-end input))
            (when (:selected? prev) (.blur input)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; special (semantic) views
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare node-view seq-view)

(defn- defn-view [data owner]
  (reify om/IRender
    (render [_]
      (let [[head name & bodies] (:children data)]
        (dom/div #js {:className (class-list data)}
         (om/build node-view head)
         (om/build node-view name)
         (apply dom/div #js {:className "runoff-children"}
          (om/build-all node-view bodies)))))))

(defn- if-view [data owner]
  (reify om/IRender
    (render [_]
      (if (> (count (p/tree->str data)) MAX_CHARS)
          (let [[head test & clauses] (:children data)]
            (dom/div #js {:className (class-list data)}
             (om/build node-view head)
             (om/build node-view test)
             (apply dom/div
              #js {:className "runoff-children"
                   :style #js {:margin-left "2rem"}}
              (om/build-all node-view
               (map #(merge % {:enclosing owner}) clauses)))))
          (om/build seq-view data)))))

(def ^:private specials
  {"defn"  defn-view
   "defn-" defn-view
   "if"    if-view})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; collection, generic, root views
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- head-size [items]
  (let [itemc (count items)]
    (loop [offset 1 idxs (range itemc)]
      (if-let [idx (first idxs)]
        (let [item-width (count (p/tree->str (nth items idx)))
              offset' (+ offset 1 item-width)]
          (if (> offset' MAX_CHARS) idx (recur offset' (rest idxs))))
        itemc))))

(defn- seq-view [data owner]
  (reify om/IRender
    (render [_]
      (let [children (:children data)
            headc (head-size children)]
        (apply dom/div #js {:className (class-list data)}
         (concat
          (om/build-all node-view
           (->> (take headc children)
                (map #(merge % {:enclosing owner}))))
          [(apply dom/div #js {:className "runoff-children"}
            (om/build-all node-view
             (->> (drop headc children)
                  (map #(merge % {:enclosing owner})))))]))))))

(defn- coll-view [data owner]
  (reify om/IRender
    (render [_]
      (apply dom/div #js {:className (class-list data)}
        (om/build-all node-view (:children data))))))

(defn- node-view [data owner]
  (reify
    om/IRender
    (render [_]
      (om/build
       (cond
        (= (:type data) :seq)
        (or (specials (:text (first (:children data)))) seq-view)
        (p/coll-node? data) coll-view
        (= (:type data) :string-content) string-content-view
        :else token-view)
       data))
    om/IDidUpdate
    (did-update [_ _ _]
      (when-let [enclosing-view (:enclosing data)]
        (om/refresh! enclosing-view)))))

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

(defn- handle-command-bar-key [command-chan ev]
  (condp = (key-data ev)
    #{:ENTER} (let [input (.-target ev)]
                (async/put! command-chan (string/split (.-value input) #"\s+"))
                (set! (.-value input) "")
                (.blur input))
    #{:ESC} (.. ev -target blur) nil)
  (.stopPropagation ev)) ; allow default behavior instead of keybound

(defn command-bar-view [_ owner]
  (reify om/IRender
    (render [_]
      (dom/input
        #js {:id "command-bar"
             :onKeyDown (partial handle-command-bar-key
                         (om/get-shared owner :command-chan))}))))
