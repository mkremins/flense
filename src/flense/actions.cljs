(ns flense.actions
   (:require [flense.actions.clipboard :as clipboard]
             [flense.actions.clojure :as clojure]
             [flense.actions.history :as history]
             [flense.actions.movement :as movement]
             [flense.actions.paredit :as paredit]
             [flense.actions.text :as text]))

(def default-actions
  (merge clipboard/actions
         clojure/actions
         history/actions
         movement/actions
         paredit/actions
         text/actions))
