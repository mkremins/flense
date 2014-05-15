(ns flense.ui.editor
  (:refer-clojure :exclude [chars rem])
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

(defn- in-viewport? [el]
  (let [rect (.getBoundingClientRect el)]
    (and (>= (.-top rect) 0)
         (<= (.-bottom rect) (.-innerHeight js/window)))))

(defn- scroll-viewport-to-contain [el]
  (.scrollTo js/window 0
             (+ (.-offsetTop el)
                (- (.-innerHeight js/window))
                (.-offsetHeight el)
                60)))

(def ^:private MAX_CHARS 72)

(defn- line-count [text]
  (inc (int (/ (count text) (- MAX_CHARS 2)))))

(defn- px [n]
  (str n "px"))

(defn- rem [n]
  (str n "rem"))

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
               :style #js {:height (rem (* 1.2 (line-count text)))
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
;; seq views
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare node-view)

(defn- chars [tree]
  (count (p/tree->str tree)))

(defn- head-count [items]
  (let [itemc (count items)]
    (loop [offset 1 idxs (range itemc)]
      (if-let [idx (first idxs)]
        (let [item-width (chars (nth items idx))
              offset' (+ offset 1 item-width)]
          (if (> offset' MAX_CHARS) idx (recur offset' (rest idxs))))
        itemc))))

(defn- indent-size [tree]
  (let [head (first (:children tree))]
    (if (#{:keyword :symbol} (:type head))
        (+ 2 (chars head))
        2)))

(defn- seq-view*
  "Constructs and returns a seq view whose contents are formatted according to
   the format specification passed as `opts`."
  [data owner opts]
  (reify om/IRender
    (render [_]
      (let [{:keys [always-multiline? fixed-head-count indent]
             :or   {indent (indent-size data)}} opts
            merge-props (when-not always-multiline? {:enclosing owner})
            multiline?  (or always-multiline? (> (chars data) MAX_CHARS))  
            children    (:children data)
            headc (or (when multiline? fixed-head-count) (head-count children))
            heads (map #(merge % merge-props) (take headc children))
            tails (map #(merge % merge-props) (drop headc children))]
        (apply dom/div #js {:className (class-list data)}
         (concat (om/build-all node-view heads)
                 [(apply dom/div
                   #js {:className "runoff-children"
                        :style #js {:margin-left (rem (/ indent 2))}}
                   (om/build-all node-view tails))]))))))

(def ^:private special-formats
  "Formatting options for clojure.core macros that are commonly indented in a
   manner not consistent with the standard function call indentation style."
  {"defmacro"  {:fixed-head-count 2 :indent 2 :always-multiline? true}
   "defn"      {:fixed-head-count 2 :indent 2 :always-multiline? true}
   "defn-"     {:fixed-head-count 2 :indent 2 :always-multiline? true}
   "do"        {:fixed-head-count 2 :indent 4}
   "if"        {:fixed-head-count 2 :indent 4}
   "if-let"    {:fixed-head-count 2 :indent 2 :always-multiline? true}
   "let"       {:fixed-head-count 2 :indent 2 :always-multiline? true}
   "loop"      {:fixed-head-count 2 :indent 2 :always-multiline? true}
   "ns"        {:fixed-head-count 2 :indent 2 :always-multiline? true}
   "when"      {:fixed-head-count 2 :indent 2}
   "when-let"  {:fixed-head-count 2 :indent 2 :always-multiline? true}})

(defn- seq-view [data owner]
  (reify om/IRender
    (render [_]
      (om/build seq-view* data
       {:opts (get special-formats (-> data :children first :text))}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; collection, generic, root views
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
       (cond (= (:type data) :seq) seq-view
             (p/coll-node? data) coll-view
             (= (:type data) :string-content) string-content-view
             :else token-view)
       data))
    om/IDidUpdate
    (did-update [_ _ _]
      (when-let [enclosing-view (:enclosing data)]
        (om/refresh! enclosing-view))
      (when (:selected? data)
        (let [el (om/get-node owner)]
          (when-not (in-viewport? el) (scroll-viewport-to-contain el)))))))

(defn editor-view [app-state owner]
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
        (apply dom/div #js {:className "editor"}
          (om/build-all node-view (:children tree)))))))
