(ns flense.editor
  (:refer-clojure :exclude [rem])
  (:require [cljs.core.async :as async]
            [clojure.string :as str]
            [flense.actions.history :as history]
            [flense.layout :as layout]
            [flense.model :as model]
            [flense.util :refer [update]]
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
  (reify om/IRender
    (render [_]
      (dom/span #js {
        :className
          (class-name
            (cond-> #{:atom (:type form)}
              (:head? form) (conj :head)
              (:selected? form) (conj :selected)
              (:collapsed-form form) (conj :macroexpanded)))
        :onClick
          #(async/put! (:nav-chan opts) (layout/path-to form))}
        (:text form)))))

(defn stringlike-view [form owner opts]
  (reify
    om/IRender
    (render [_]
      (let [text (str/replace (:text form) #"\s+" " ")
            {:keys [line-length]} opts]
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
            #(when (and (:editing? @form) (not= (.-keyCode %) 38)) ;; up key
               (.stopPropagation %))
          :onKeyPress
            #(.stopPropagation %)
          :style #js {
            :height (rem (* 1.15 (layout/text-height text line-length)))
            :width  (rem (/ (layout/text-width text line-length) 2))}
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
        (:text token)))))

(defn form-view [form owner opts]
  (reify om/IRender
    (render [_]
      (apply dom/div #js {:className "toplevel"}
        (for [line (layout/->lines form (:line-length opts))]
          (apply dom/div #js {:className "line"}
            (for [token line]
              (condp apply [token]
                layout/spacer? (dom/span #js {:className "spacer"} (:text token))
                layout/delimiter? (om/build delimiter-view token {:opts opts})
                model/stringlike? (om/build stringlike-view token {:opts opts})
                (om/build atom-view token {:opts opts})))))))))

(defn perform [f tags]
  (fn [loc]
    (if-let [loc' (f loc)]
      (cond-> loc' (not (:history tags)) (history/save loc))
      loc)))

(defn editor-view [document owner opts]
  (reify
    om/IInitState
    (init-state [_]
      {:nav-chan (async/chan)})
    om/IWillMount
    (will-mount [_]
      (go-loop []
        (when-let [action (<! (:edit-chan opts))]
          (let [{:keys [tags] :or {tags #{}}} (meta action)]
            (om/transact! document [] (perform action tags) tags))
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
            {:opts (-> opts (update :line-length (fnil identity 72))
                            (assoc :nav-chan nav-chan))}))))))
