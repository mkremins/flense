(ns flense.core
  (:require [flense.render :as render]
            [flense.util :refer [form->tree]]
            [flense.zip :as z]
            [goog.events.KeyCodes :as key]
            [om.core :as om]))

(enable-console-print!)

(def app-state
  (atom
    (z/->BoundedZipper
      (z/->SimpleZipper [0]
        {:children
         [(form->tree '(fn greet [name] (str "Hello, " name "!")))]}))))

(def placeholder
  (form->tree '...))

(defn insert-form [loc]
  (z/down (z/insert-right loc placeholder)))

(defn remove-node [loc]
  (let [node (z/node loc)]
    (if (= (:form node) '...)
        (z/remove loc)
        (z/replace loc placeholder))))

(def templates
  {'def  '(def ... ...)
   'defn '(defn ... [...] ...)
   'fn   '(fn [...] ...)
   'let  '(let [... ...] ...)})

(defn expand-node [loc]
  (let [node (z/node loc)]
    (if-let [template (templates (:form node))]
      (z/replace loc (form->tree template))
      loc)))

;; keybinds

(def default-binds
  {key/BACKSPACE  remove-node
   key/DOWN       z/down
   key/LEFT       z/left
   key/RIGHT      z/right
   key/SPACE      insert-form
   key/TAB        expand-node
   key/UP         z/up})

(defn handle-key [keybinds ev]
  (when-let [exec-bind (get keybinds (.-keyCode ev))]
    (.preventDefault ev)
    (swap! app-state exec-bind)))

;; application setup and wiring

(defn init []
  (om/root render/root-view app-state
           {:target (.getElementById js/document "flense-parent")})
  (.addEventListener js/window "keydown" (partial handle-key default-binds)))

(init)
