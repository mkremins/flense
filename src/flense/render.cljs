(ns flense.render
  (:require [clojure.string :as string]
            [flense.util :refer [coll-node? form->tree]]
            [flense.zip :as z]
            [goog.events.KeyCodes :as key]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(defn- class-list [{:keys [selected? type] :as node}]
  (->> [(name type)
        (if (coll-node? node) "coll" "atom")
        (when selected? "selected")]
       (string/join " ")
       string/trimr))

(defn- parse-char [text]
  {:type :char :form (subs text 1)})

(defn- parse-keyword [text]
  {:type :keyword :form (keyword (subs text 1))})

(defn- parse-regex [text]
  {:type :regex :form (re-pattern (subs text 2 (dec (count text))))})

(defn- parse-string [text]
  {:type :string :form (subs text 1 (dec (count text)))})

(defn- parse-symbol-or-number [text]
  (let [number (js/parseFloat text)]
    (if (js/isNaN number)
        {:type :symbol :form (symbol text)}
        {:type :number :form number})))

(defn- parse-atom [text]
  (let [init-ch (first text)]
    (cond
      (= text "false") {:type :bool :form false}
      (= text "nil")   {:type :nil  :form nil}
      (= text "true")  {:type :bool :form true}
      (= init-ch \\)   (parse-char text)
      (= init-ch \:)   (parse-keyword text)
      (= init-ch \#)   (parse-regex text)
      (= init-ch \")   (parse-string text)
      :else            (parse-symbol-or-number text))))

(defn- atom-width [form]
  (let [tester (.getElementById js/document "width-tester")]
    (set! (.-textContent tester) (pr-str form))
    (str (inc (.-clientWidth tester)) "px")))

(defn- wrap-coll [type node]
  {:type type
   :form (case type
           :map {node '...}
           :seq (list node)
           :vec [node])
   :children [node]})

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
      (om/transact! data [] (partial wrap-coll :seq) :insert-coll))

    key/OPEN_SQUARE_BRACKET
    (do (.preventDefault ev)
        (om/transact! data []
                      (partial wrap-coll (if (.-shiftKey ev) :map :vec))
                      :insert-coll))
    
    nil)) ; deliberate no-op

(defn- atom-view [node owner]
  (reify
    om/IRender
    (render [_]
      (dom/input
        #js {:className (class-list node)
             :onChange  #(om/update! node (parse-atom (.. % -target -value)))
             :onKeyDown #(handle-key % node)
             :style #js {:width (atom-width (:form node))}
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
