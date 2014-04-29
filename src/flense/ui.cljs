(ns flense.ui
  (:require [cljs.core.async :as async]
            [clojure.string :as string]
            [flense.edit :as e]
            [flense.parse :as p]
            [flense.zip :as z]
            [goog.events.KeyCodes :as key]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(def ^:dynamic *root-cursor*)

(defn- class-list [{:keys [break-after? selected? type] :as node}]
  (string/join " "
   [(name type)
    (if (p/coll-node? node) "coll" "atom")
    (when break-after? "break-after")
    (when selected? "selected")]))

(def ^:private max-chars 60)

(defn- line-count [text]
  (reduce (fn [linec line]
            (+ linec (inc (int (/ (count line) (- max-chars 2))))))
          0 (string/split text #"\n")))

(defn- px [n]
  (str n "px"))

(defn- raw-render-width [raw-string]
  (let [tester (.getElementById js/document "width-tester")]
    (set! (.-textContent tester) raw-string)
    (inc (.-clientWidth tester))))

(def ^:private char-width
  (raw-render-width " "))

(def ^:private max-width
  (raw-render-width (string/join (repeat max-chars " "))))

(defn- render-width [node]
  (raw-render-width (p/tree->str node)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; atom views
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- handle-key [ev data]
  (condp = (.-keyCode ev)
    key/BACKSPACE
    (let [input (.-target ev)]
      (when (or (not= (.-selectionStart input) 0)
                (not= (.-selectionEnd input) (count (.-value input))))
        (.stopPropagation ev)))

    key/NINE
    (when (.-shiftKey ev)
      (.preventDefault ev)
      (om/transact! data [] (partial e/wrap-sexp :seq) :wrap-coll))

    key/OPEN_SQUARE_BRACKET
    (do (.preventDefault ev)
        (om/transact! data []
                      (partial e/wrap-sexp (if (.-shiftKey ev) :map :vec))
                      :wrap-coll))

    key/SINGLE_QUOTE
    (when (.-shiftKey ev)
      (.preventDefault ev)
      (om/transact! data [] e/wrap-string :wrap-coll))
    
    nil)) ; deliberate no-op

(defn- atom-view [node owner]
  (reify
    om/IRender
    (render [_]
      (dom/input
        #js {:className (class-list node)
             :onChange  #(om/update! node (p/parse-atom (.. % -target -value)))
             :onKeyDown #(handle-key % node)
             :style #js {:width (px (render-width node))}
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
  (condp = (.-keyCode ev)
    key/BACKSLASH
    (do (.preventDefault ev)
        (om/transact! data :text #(str % "\\")))

    key/BACKSPACE
    (let [input (.-target ev)]
      (when (or (not= (.-selectionStart input) 0)
                (not= (.-selectionEnd input) (count (.-value input))))
        (.stopPropagation ev)))

    key/SPACE ; allow default behavior (insert space) instead of keybound
    (.stopPropagation ev)

    key/SINGLE_QUOTE
    (when (.-shiftKey ev)
      (.preventDefault ev)
      (om/transact! data :text #(str % "\"")))

    nil)) ; deliberate no-op

(defn- string-content-view [node owner]
  (reify
    om/IRender
    (render [_]
      (let [text (:text node)]
        (dom/textarea
          #js {:className (class-list node)
               :onChange  #(om/update! node :text (.. % -target -value))
               :onKeyDown #(handle-string-key % node)
               :style #js {:height (str (* 1.3 (line-count text)) "rem")
                           :width  (px (if (<= (count text) (- max-chars 2))
                                           (raw-render-width text)
                                           max-width))}
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

(defn- coll-view [node owner]
  (reify om/IRender
    (render [_]
      (apply dom/div #js {:className (class-list node)}
        (loop [rendered []
               horiz char-width
               children (:children node)]
          (if-let [child (first children)]
                  (let [width   (+ (render-width child) char-width)
                        sibling (second children)
                        break?  (when sibling
                                  (> (+ horiz width (render-width sibling))
                                     max-width))]
                    (recur
                     (conj rendered
                      (om/build node-view (assoc child :break-after? break?)))
                     (if break? (* 2 char-width) (+ horiz width))
                     (rest children)))
                  rendered))))))

(defn- node-view [node owner]
  (reify
    om/IRender
    (render [this]
      (om/build
        (cond
          (p/coll-node? node) coll-view
          (= (:type node) :string-content) string-content-view
          :else atom-view)
        node))))

(defn root-view [app-state owner]
  (reify
    om/IRender
    (render [_]
      (set! *root-cursor* app-state)
      (let [{:keys [tree]} (z/edit app-state assoc :selected? true)]
        (apply dom/div #js {:className "flense"}
          (om/build-all node-view (:children tree)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; command bar view
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- handle-command-bar-key [owner ev]
  (condp = (.-keyCode ev)
    key/ENTER
    (let [input (.-target ev)]
      (async/put!
       (om/get-shared owner :command-chan)
       (string/split (.-value input) #"\s+"))
      (set! (.-value input) "")
      (.blur input))

    key/ESC
    (.. ev -target blur)

    nil)
  (.stopPropagation ev)) ; allow default behavior instead of keybound

(defn command-bar-view [app-state owner]
  (reify om/IRender
    (render [_]
      (dom/input
        #js {:id "command-bar"
             :onKeyDown (partial handle-command-bar-key owner)}))))
