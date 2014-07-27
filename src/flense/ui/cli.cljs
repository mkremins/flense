(ns flense.ui.cli
  (:require [cljs.core.async :as async]
            [clojure.string :as string]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [phalanges.core :as phalanges]))

(defn- handle-key [command-chan ev]
  (condp = (phalanges/key-set ev)
    #{:enter} (let [input (.-target ev)]
                (async/put! command-chan (string/split (.-value input) #"\s+"))
                (set! (.-value input) "")
                (.blur input))
    #{:esc} (.. ev -target blur) nil)
  (.stopPropagation ev)) ; allow default behavior instead of keybound

(defn cli-view [_ owner]
  (reify om/IRender
    (render [_]
      (dom/input
        #js {:id "cli"
             :onKeyDown (partial handle-key
                         (om/get-shared owner :command-chan))}))))
