(ns flense.ui.editor
  (:refer-clojure :exclude [chars rem])
  (:require [cljs.core.async :as async]
            [clojure.string :as string]
            [flense.keymap :as keymap]
            [flense.parse :as p]
            [flense.util.dom :as udom :refer [rem]]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [xyzzy.core :as z])
  (:require-macros [cljs.core.async.macros :refer [go-loop]]))

(defn- class-list [data]
  (string/join " " [(name (:type data))
                    (cond (p/coll-node? data) "coll"
                          (p/stringlike-node? data) "stringlike"
                          :else "token")
                    (when (:collapsed-form data) "macroexpanded")
                    (when (:editing? data) "editing")
                    (when (:selected? data) "selected")]))

(def ^:private MAX_CHARS 72)

(defn- line-count [text]
  (inc (int (/ (count text) (- MAX_CHARS 2)))))

(defn- chars [tree]
  (count (p/tree->str tree)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; token views
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- token-view [data owner]
  (reify
    om/IRender
    (render [_]
      (dom/input
        #js {:className (class-list data)
             :onChange  #(om/update! data (p/parse-token (.. % -target -value)))
             :onKeyDown ; prevent delete keybind unless text fully selected
                        #(when (and (= (:name (keymap/bound-action %)) :paredit/remove)
                                    (not (udom/fully-selected? (.-target %))))
                           (.stopPropagation %))
             :style #js {:width (rem (/ (chars data) 2))}
             :value (:text data)}))
    om/IDidMount
    (did-mount [_]
      (when (:selected? data) (udom/focus+select (om/get-node owner))))
    om/IDidUpdate
    (did-update [_ prev _]
      (let [input (om/get-node owner)]
        (if (:selected? data)
            (when (or (not (:selected? prev)) (p/placeholder-node? data))
              (udom/focus+select input))
            (when (:selected? prev) (.blur input)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stringlike views
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- stringlike-view [data owner]
  (reify
    om/IRender
    (render [_]
      (dom/div #js {:className (class-list data)}
        (let [text (string/replace (:text data) #"\s+" " ")]
          (dom/textarea
            #js {:onChange  #(om/update! data :text (.. % -target -value))
                 :onKeyDown ; prevent keybinds (except those that end editing)
                            #(when-not (-> % keymap/bound-action :name
                                           #{:flense/text-command :move/up :paredit/insert-outside})
                               (.stopPropagation %))
                 :ref "content"
                 :style #js {:height (rem (* 1.2 (line-count text)))
                             :width  (rem (/ (min (count text) MAX_CHARS) 2))}
                 :value text}))))
    om/IDidMount
    (did-mount [_]
      (when (:editing? data)
        (let [input (om/get-node owner "content")]
          (if (p/placeholder-node? data)
            (udom/focus+select input)
            (udom/move-caret-to-end input)))))
    om/IDidUpdate
    (did-update [_ prev _]
      (let [input (om/get-node owner "content")]
        (if (:editing? data)
          (when-not (:editing? prev) (udom/move-caret-to-end input))
          (when (:editing? prev) (.blur input)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; seq views
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare node-view)

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
      (let [{:keys [inlineable? fixed-head-count indent]
             :or   {indent (indent-size data)}} opts
            merge-props (when inlineable? {:enclosing owner})
            multiline?  (or (not inlineable?) (> (chars data) MAX_CHARS))
            children    (map #(merge % merge-props) (:children data))
            headc (or (when multiline? fixed-head-count) (head-count children))]
        (apply dom/div #js {:className (class-list data)}
         (concat (om/build-all node-view (take headc children))
                 [(apply dom/div
                   #js {:className "runoff-children"
                        :style #js {:margin-left (rem (/ indent 2))}}
                   (om/build-all node-view (drop headc children)))]))))))

(def ^:private special-formats
  "Formatting options for clojure.core macros that are commonly indented in a
   manner not consistent with the standard function call indentation style."
  {"defmacro"  {:fixed-head-count 2 :indent 2}
   "defn"      {:fixed-head-count 2 :indent 2}
   "defn-"     {:fixed-head-count 2 :indent 2}
   "do"        {:fixed-head-count 2}
   "if"        {:fixed-head-count 2 :indent 2 :inlineable? true}
   "if-let"    {:fixed-head-count 2 :indent 2}
   "let"       {:fixed-head-count 2 :indent 2}
   "loop"      {:fixed-head-count 2 :indent 2}
   "ns"        {:fixed-head-count 2 :indent 2}
   "when"      {:fixed-head-count 2 :indent 2 :inlineable? true}
   "when-let"  {:fixed-head-count 2 :indent 2}})

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
             (p/stringlike-node? data) stringlike-view
             :else token-view)
       data))
    om/IDidUpdate
    (did-update [_ _ _]
      (when-let [enclosing-view (:enclosing data)]
        (om/refresh! enclosing-view))
      (when (:selected? data)
        (let [el (om/get-node owner)]
          (when-not (udom/in-view? el) (udom/scroll-into-view el)))))))

(defn editor-view [app-state owner]
  (reify
    om/IWillMount
    (will-mount [_]
      (let [edit-chan (om/get-shared owner :edit-chan)]
        (go-loop []
          (let [{:keys [cases]} (<! edit-chan)]
            (when-let [case (first (filter #((:pred %) @app-state) cases))]
              (om/transact! app-state [] (:edit case) (:tags case))))
          (recur))))
    om/IRender
    (render [_]
      (let [{:keys [tree]} (z/edit app-state assoc :selected? true)]
        (apply dom/div #js {:className "editor"}
          (om/build-all node-view (:children tree)))))))
