(ns flense.ui.error
  (:require [cljs.core.async :as async :refer [alts!]]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true])
  (:require-macros [cljs.core.async.macros :refer [go-loop]]))

(defn error-bar-view [_ owner]
  (reify
    om/IInitState
    (init-state [_]
      {:message "" :visible? false})
    om/IWillMount
    (will-mount [_]
      (let [error-chan (om/get-shared owner :error-chan)]
        (go-loop [timeout (async/chan)]
          (let [[msg ch] (alts! [error-chan timeout])]
            (condp = ch
              error-chan
              (do (om/set-state! owner :message msg)
                  (om/set-state! owner :visible? true)
                  (recur (async/timeout 5000)))
              timeout
              (do (om/set-state! owner :visible? false)
                  (recur (async/chan))))))))
    om/IRenderState
    (render-state [_ state]
      (dom/div
       #js {:className (if (:visible? state) "visible" "hidden")
            :id "error-bar"}
       (:message state)))))
