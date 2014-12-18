(ns flense.editor
  (:refer-clojure :exclude [atom rem])
  (:require [clojure.string :as str]
            [flense.actions.completions :as completions]
            [flense.actions.history :as hist]
            [flense.layout :as layout]
            [flense.model :as model]
            [om.core :as om :include-macros true]
            [om.dom :as dom]
            [xyzzy.core :as z]))

;; DOM utils

(defn class-name [classes]
  (->> classes (map name) (str/join " ")))

(defn move-caret-to-end [input]
  (let [idx (count (.-value input))]
    (set! (.-selectionStart input) idx)
    (set! (.-selectionEnd input) idx)))

(defn rem [n]
  (str n "rem"))

;; Om components

(defn completions [form owner]
  (om/component
    (let [{:keys [completions selected-completion]} form
          selected-completion (or selected-completion 0)]
      (apply dom/ul #js {:className "completions"}
        (for [i (range (count completions))
              :let [selected? (= i selected-completion)
                    [type form] (nth completions i)]]
          (dom/li #js {
            :className
              (class-name
                (cond-> #{:completion type} selected? (conj :selected)))}
            (pr-str form)))))))

(defn atom [form owner opts]
  (om/component
    (dom/div #js {
      :className
        (class-name
          (cond-> #{:atom (:type form)}
            (:head? form) (conj :head)
            (:selected? form) (conj :selected)
            (:collapsed-form form) (conj :macroexpanded)))
      :onClick
        #((:nav-cb opts) (:path @form))}
      (when (:selected? form) (om/build completions form))
      (dom/span nil (:text form)))))

(defn stringlike [form owner opts]
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
            #((:nav-cb opts) (:path @form))
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
            (doto input .focus .select)
            (move-caret-to-end input)))))
    om/IDidUpdate
    (did-update [_ prev _]
      (let [input (om/get-node owner)]
        (if (:editing? form)
          (when-not (:editing? prev)
            (move-caret-to-end input))
          (when (:editing? prev)
            (.blur input)))))))

(defn delimiter [token owner opts]
  (om/component
    (dom/span #js {
      :className (class-name (:classes token))
      :onClick #((:nav-cb opts) @(:path token))}
      (:text token))))

(defn top-level-form [form owner opts]
  (om/component
    (apply dom/div #js {:className "toplevel"}
      (for [line (layout/->lines form (:line-length opts))]
        (apply dom/div #js {:className "line"}
          (for [token line]
            (condp apply [token]
              layout/spacer? (dom/span #js {:className "spacer"} (:text token))
              layout/delimiter? (om/build delimiter token {:opts opts})
              model/stringlike? (om/build stringlike token {:opts opts})
              (om/build atom token {:opts opts}))))))))

(defn perform
  "Given an `action` function, returns a wrapper action that will perform
  necessary bookkeeping before and after executing `action` itself. Generally,
  you should use `perform` to wrap any action you intend to execute on a Flense
  editor state using `om.core/transact!` or `cljs.core/swap!`."
  [action]
  (fn [loc]
    (if-let [loc' (action loc)]
      (cond-> loc' (not (#{hist/redo hist/undo} action)) (hist/save loc)
                   :always completions/update-completions
                   :always (update :tree model/annotate-paths))
      loc)))

(defn editor [document owner opts]
  (om/component
    (let [{:keys [tree]} (z/edit document assoc :selected? true)
          nav-cb (fn [path]
                   (om/transact! document []
                     #(-> % (z/edit dissoc :editing?) (assoc :path path))))]
      (apply dom/div #js {:className "flense"}
        (om/build-all top-level-form (:children tree)
          {:opts (-> opts (update :line-length (fnil identity 72))
                          (assoc :nav-cb nav-cb))})))))
