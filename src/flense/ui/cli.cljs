(ns flense.ui.cli
  (:require [cljs.core.async :as async]
            [clojure.string :as string]
            [flense.keyboard :refer [key-data]]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(defn- handle-key [command-chan ev]
  (condp = (key-data ev)
    #{:ENTER} (let [input (.-target ev)]
                (async/put! command-chan (string/split (.-value input) #"\s+"))
                (set! (.-value input) "")
                (.blur input))
    #{:ESC} (.. ev -target blur) nil)
  (.stopPropagation ev)) ; allow default behavior instead of keybound

(defn cli-view [_ owner]
  (reify om/IRender
    (render [_]
      (dom/input
        #js {:id "cli"
             :onKeyDown (partial handle-key
                         (om/get-shared owner :command-chan))}))))
