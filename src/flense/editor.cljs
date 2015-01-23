(ns flense.editor
  (:require [clojure.string :as str]
            [flense.actions.completions :as completions]
            [flense.actions.history :as hist]
            [flense.layout :as layout]
            [flense.model :as model]
            [om.core :as om]
            [om-tools.core :refer-macros [defcomponent]]
            [om-tools.dom :as dom :include-macros true]
            [xyzzy.core :as z]))

(defcomponent completions [form owner]
  (render [_]
    (let [{:keys [completions selected-completion]} form
          selected-completion (or selected-completion 0)]
      (dom/ul {:class "completions"}
        (for [i (range (count completions))
              :let [[type form] (nth completions i)]]
          (dom/li {:class (cond-> (str "completion " (name type))
                                  (= i selected-completion) (str " selected"))}
            (pr-str form)))))))

(defcomponent atom* [form owner opts]
  (render [_]
    (dom/div {:class (cond-> (str "atom " (name (:type form)))
                             (:head? form) (str " head")
                             (:selected? form) (str " selected")
                             (:collapsed-form form) (str " macroexpanded"))
              :on-click #((:nav-cb opts) (:path @form))}
      (when (:selected? form) (om/build completions form))
      (dom/span (:text form)))))

(defcomponent stringlike [form owner opts]
  (render [_]
    (dom/div {:class (cond-> (str "stringlike " (name (:type form)))
                             (:editing? form) (str " editing")
                             (:selected? form) (str " selected"))
              :style {:max-width (str (/ (:max-width form) 2) "rem")}}
      (for [i (range (count (:text form)))]
        (dom/span {:class (when (= i (:char-idx form)) "selected")
                   :on-click #((:nav-cb opts) (:path @form) i)}
          (nth (:text form) i))))))

(defcomponent delimiter [token owner opts]
  (render [_]
    (dom/span {:class (str/join " " (map name (:classes token)))
               :on-click #((:nav-cb opts) @(:path token))}
      (:text token))))

(defcomponent top-level-form [form owner opts]
  (render [_]
    (dom/div {:class "toplevel"}
      (for [line (layout/->lines form (:line-length opts))]
        (dom/div {:class "line"}
          (for [token line]
            (condp #(%1 %2) token
              layout/spacer? (dom/span {:class "spacer"} (:text token))
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

(defn- nav-cb [document]
  (fn ([path]
        (om/transact! document []
          #(-> % (z/dissoc :editing?) (assoc :path path))))
      ([path idx]
        (om/transact! document []
          #(-> % (z/dissoc :editing?) (assoc :path path)
                 (z/assoc :editing? true :char-idx idx))))))

(defcomponent editor [document owner opts]
  (render [_]
    (let [{:keys [tree]} (z/assoc document :selected? true)]
      (dom/div {:class "flense"}
        (om/build-all top-level-form (:children tree)
          {:opts (-> opts (update :line-length (fnil identity 72))
                          (assoc :nav-cb (nav-cb document)))})))))
