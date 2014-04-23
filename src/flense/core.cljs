(ns flense.core
  (:require [flense.edit :as e]
            [flense.ui :as ui]
            [flense.util :refer [form->tree placeholder]]
            [flense.zip :as z]
            [goog.events.KeyCodes :as key]
            [om.core :as om]))

(enable-console-print!)

(def app-state
  (atom
   {:path [0]
    :tree {:children
           [(form->tree '(fn greet [name] (str "Hello, " name "!")))]}}))

;; keybinds

(def default-binds
  {key/BACKSPACE  e/delete-sexp
   key/DOWN       z/down-or-stay
   key/LEFT       z/left-or-wrap
   key/RIGHT      z/right-or-wrap
   key/SPACE      #(e/insert-right % placeholder)
   key/UP         z/up-or-stay})

(def shift-binds
  {key/THREE  e/toggle-dispatch})

(defn handle-key [ev]
  (let [keybinds (if (.-shiftKey ev) shift-binds default-binds)]
    (when-let [exec-bind (get keybinds (.-keyCode ev))]
      (.preventDefault ev)
      (om/transact! ui/*root-cursor* exec-bind))))

;; application setup and wiring

(defn init []
  (om/root ui/root-view app-state
           {:target (.getElementById js/document "flense-parent")
            :tx-listen #(when (= (:tag %) :insert-coll)
                          (om/transact! ui/*root-cursor* z/down))})
  (.addEventListener js/window "keydown" handle-key))

(init)
