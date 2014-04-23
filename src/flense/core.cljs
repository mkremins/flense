(ns flense.core
  (:require [flense.edit :as e]
            [flense.history :as hist]
            [flense.parse :as p]
            [flense.ui :as ui]
            [flense.zip :as z]
            [goog.events.KeyCodes :as key]
            [om.core :as om]))

(enable-console-print!)

(def app-state
  (atom
   {:path [0]
    :tree {:children
           [(p/form->tree '(fn greet [name] (str "Hello, " name "!")))]}}))

;; keybinds

(def default-binds
  {key/BACKSPACE  e/delete-sexp
   key/DOWN       z/down-or-stay
   key/LEFT       z/left-or-wrap
   key/RIGHT      z/right-or-wrap
   key/SPACE      #(e/insert-right % p/placeholder)
   key/UP         z/up-or-stay})

(def meta-binds
  {key/Y  hist/redo
   key/Z  hist/undo})

(def meta-ctrl-binds
  {key/LEFT   e/barf-left
   key/RIGHT  e/barf-right})

(def meta-shift-binds
  {key/LEFT   e/slurp-left
   key/RIGHT  e/slurp-right})

(def shift-binds
  {key/LEFT   z/backward
   key/RIGHT  z/forward
   key/THREE  e/toggle-dispatch})

(defn handle-key [ev]
  (let [keybinds
        (cond (and (.-metaKey ev) (.-ctrlKey ev))  meta-ctrl-binds
              (and (.-metaKey ev) (.-shiftKey ev)) meta-shift-binds
              (.-metaKey ev)  meta-binds
              (.-shiftKey ev) shift-binds
              :else default-binds)]
    (when-let [exec-bind (get keybinds (.-keyCode ev))]
      (.preventDefault ev)
      (om/transact! ui/*root-cursor* [] exec-bind
                    (when (#{hist/redo hist/undo} exec-bind) ::hist/ignore)))))

;; application setup and wiring

(defn- handle-tx [{:keys [new-state tag]}]
  (when (= tag :insert-coll)
    (om/transact! ui/*root-cursor* [] z/down ::hist/ignore))
  (when-not (= tag ::hist/ignore)
    (hist/push-state! new-state)))

(defn init []
  (hist/push-state! @app-state)
  (om/root ui/root-view app-state
           {:target (.getElementById js/document "flense-parent")
            :tx-listen handle-tx})
  (.addEventListener js/window "keydown" handle-key))

(init)
