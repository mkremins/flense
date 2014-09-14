(ns flense.editor
  (:refer-clojure :exclude [rem])
  (:require [cljs.core.async :as async]
            [clojure.string :as str]
            [flense.layout :as layout]
            [flense.model :as model]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [xyzzy.core :as z])
  (:require-macros [cljs.core.async.macros :refer [go-loop]]))

;; DOM utils

(defn class-name [classes]
  (->> classes (map name) (str/join " ")))

(defn focus+select [input]
  (doto input .focus .select))

(defn move-caret-to-end [input]
  (let [idx (count (.-value input))]
    (set! (.-selectionStart input) idx)
    (set! (.-selectionEnd input) idx)))

(defn rem [n]
  (str n "rem"))

;; Om components

(defn atom-view [form owner opts]
  (reify
    om/IRender
    (render [_]
      (dom/input #js {
        :className
          (class-name
           (cond-> #{:atom (:type form)}
             (:head? form) (conj :head)
             (:selected? form) (conj :selected)
             (:collapsed-form form) (conj :macroexpanded)))
        :onChange
          #(om/update! form (model/string->atom (.. % -target -value)))
        :onClick
          #(async/put! (:nav-chan opts) (layout/path-to form))
        :onKeyDown
          #(when-not ((:propagate-keypress? opts) % @form)
             (.stopPropagation %))
         :style #js {:width (rem (/ (layout/chars form) 2))}
         :value (:text form)}))
    om/IDidMount
    (did-mount [_]
      (when (:selected? form)
        (focus+select (om/get-node owner))))
    om/IDidUpdate
    (did-update [_ prev _]
      (let [input (om/get-node owner)]
        (if (:selected? form)
          (when (or (not (:selected? prev)) (model/placeholder? form))
            (focus+select input))
          (when (:selected? prev)
            (.blur input)))))))

(defn stringlike-view [form owner opts]
  (reify
    om/IRender
    (render [_]
      (let [text (str/replace (:text form) #"\s+" " ")]
        (dom/textarea #js {
          :className
            (class-name
             (cond-> #{:stringlike (:type form)}
               (:editing? form) (conj :editing)
               (:selected? form) (conj :selected)))
          :onChange
            #(om/update! form :text (.. % -target -value))
          :onClick
            #(async/put! (:nav-chan opts) (layout/path-to form))
          :onKeyDown
            #(when-not ((:propagate-keypress? opts) % @form)
               (.stopPropagation %))
            :style #js {
              :height (rem (* 1.15 (layout/text-height text)))
              :width  (rem (/ (layout/text-width text) 2))}
            :value text})))
    om/IDidMount
    (did-mount [_]
      (when (:editing? form)
        (let [input (om/get-node owner)]
          (if (model/placeholder? form)
            (focus+select input)
            (move-caret-to-end input)))))
    om/IDidUpdate
    (did-update [_ prev _]
      (let [input (om/get-node owner)]
        (if (:editing? form)
          (when-not (:editing? prev)
            (move-caret-to-end input))
          (when (:editing? prev)
            (.blur input)))))))

(defn delimiter-view [token owner opts]
  (reify om/IRender
    (render [_]
      (dom/span #js {
        :className (class-name (:classes token))
        :onClick #(async/put! (:nav-chan opts) (:path token))}
        (:content token)))))

(defn form-view [form owner opts]
  (reify om/IRender
    (render [_]
      (apply dom/div #js {:className "toplevel"}
        (for [line (layout/->lines form)]
          (apply dom/div #js {:className "line"}
            (for [token line]
              (condp apply [token]
                layout/spacer? (dom/span #js {:className "spacer"} (:content token))
                layout/delimiter? (om/build delimiter-view token {:opts opts})
                model/stringlike? (om/build stringlike-view token {:opts opts})
                (om/build atom-view token {:opts opts})))))))))

(defn editor-view [document owner opts]
  (reify
    om/IInitState
    (init-state [_]
      {:nav-chan (async/chan)})
    om/IWillMount
    (will-mount [_]
      (go-loop []
        (when-let [action (<! (:edit-chan opts))]
          (when ((:pred action) @document)
            (om/transact! document [] (:edit action) (:tags action)))
          (recur)))
      (go-loop []
        (when-let [new-path (<! (om/get-state owner :nav-chan))]
          (om/transact! document []
            #(-> % (z/edit dissoc :editing?) (assoc :path new-path)))
          (recur))))
    om/IWillUnmount
    (will-unmount [_]
      (async/close! (om/get-state owner :nav-chan)))
    om/IRenderState
    (render-state [_ {:keys [nav-chan]}]
      (let [{:keys [tree]} (z/edit document assoc :selected? true)]
        (apply dom/div #js {:className "flense"}
          (om/build-all form-view (:children tree)
            {:opts (-> opts (select-keys [:propagate-keypress?])
                            (assoc :nav-chan nav-chan))}))))))
