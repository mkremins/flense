(ns flense.core
  (:require [flense.keys :as keys]
            [flense.render :as render]
            [flense.util :refer [form->tree]]
            [flense.zip :as z]
            [om.core :as om]))

(enable-console-print!)

(def app-state
  (atom
    (z/->BoundedZipper
      (z/->SimpleZipper [0]
        {:children
         [(form->tree '(fn greet [name] (str "Hello, " name "!")))]}))))

(defn insert-form [loc]
  (z/down (z/insert-right loc (form->tree '...))))

;(defn remove-node [loc]
;  (if (zip/up loc) (zip/remove loc) loc))

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
  { ;; simple navigation commands
   :DOWN  z/down
   :LEFT  z/left
   :RIGHT z/right
   :UP    z/up
    ;; structure editing
   ;:DEL  remove-node
   :SPACE insert-form
   :TAB   expand-node})

(defn handle-key [keybinds ev]
  (when-let [exec-bind (get keybinds (keys/ev->key ev))]
    (.preventDefault ev)
    (swap! app-state exec-bind)))

;; application setup and wiring

(defn init []
  (om/root render/root-view app-state
           {:target (.getElementById js/document "flense-parent")})
  (.addEventListener js/window "keydown" (partial handle-key default-binds)))

(init)
