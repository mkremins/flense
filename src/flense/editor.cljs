(ns flense.editor
  (:require [clojure.string :as str]
            [flense.actions.completions :as completions]
            [flense.actions.history :as hist]
            [flense.layout :as layout]
            [flense.model :as model]
            [om.core :as om :include-macros true]
            [om.dom :as dom]
            [xyzzy.core :as z]))

(defn- completions [form owner]
  (om/component
    (let [{:keys [completions selected-completion]} form
          selected-completion (or selected-completion 0)]
      (apply dom/ul #js {:className "completions"}
        (for [i (range (count completions))
              :let [selected? (= i selected-completion)
                    [type form] (nth completions i)]]
          (dom/li #js {:className (cond-> (str "completion " (name type))
                                          selected? (str " selected"))}
            (pr-str form)))))))

(defn- atom* [form owner opts]
  (om/component
    (dom/div #js {
      :className (cond-> (str "atom " (name (:type form)))
                         (:head? form) (str " head")
                         (:selected? form) (str " selected")
                         (:collapsed-form form) (str " macroexpanded"))
      :onClick #((:nav-cb opts) (:path @form))}
      (when (:selected? form) (om/build completions form))
      (dom/span nil (:text form)))))

(defn- stringlike-styles [text line-length]
  (let [charc (count text)]
    #js {:height (str (* 1.15 (inc (int (/ charc (- line-length 2))))) "rem")
         :width  (str (/ (min charc line-length) 2) "rem")}))

(defn- move-caret-to-end [input]
  (let [idx (count (.-value input))]
    (set! (.-selectionStart input) idx)
    (set! (.-selectionEnd input) idx)))

(defn- stringlike [form owner opts]
  (reify
    om/IRender
    (render [_]
      (let [text (str/replace (:text form) #"\s+" " ")]
        (dom/textarea #js {
          :className (cond-> (str "stringlike " (name (:type form)))
                             (:editing? form) (str " editing")
                             (:selected? form) (str " selected"))
          :onChange #(om/update! form :text (.. % -target -value))
          :onClick #((:nav-cb opts) (:path @form))
          :onKeyDown #(when (and (:editing? @form) (not= (.-keyCode %) 38))
                        (.stopPropagation %))
          :onKeyPress #(.stopPropagation %)
          :style (stringlike-styles text (:line-length opts))
          :value text})))
    om/IDidMount
    (did-mount [_]
      (when (:editing? form)
        (if (model/placeholder? form)
          (doto (om/get-node owner) .focus .select)
          (move-caret-to-end (om/get-node owner)))))
    om/IDidUpdate
    (did-update [_ prev _]
      (if (:editing? form)
        (when-not (:editing? prev) (move-caret-to-end (om/get-node owner)))
        (when (:editing? prev) (.blur (om/get-node owner)))))))

(defn- delimiter [token owner opts]
  (om/component
    (dom/span #js {:className (str/join " " (map name (:classes token)))
                   :onClick #((:nav-cb opts) @(:path token))}
      (:text token))))

(defn top-level-form [form owner opts]
  (om/component
    (apply dom/div #js {:className "toplevel"}
      (for [line (layout/->lines form (:line-length opts))]
        (apply dom/div #js {:className "line"}
          (for [token line]
            (condp #(%1 %2) token
              layout/spacer? (dom/span #js {:className "spacer"} (:text token))
              layout/delimiter? (om/build delimiter token {:opts opts})
              model/stringlike? (om/build stringlike token {:opts opts})
              (om/build atom* token {:opts opts}))))))))

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
