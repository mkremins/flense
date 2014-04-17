(ns flense.core
  (:require [flense.render :as render]
            [flense.util :refer [coll-node? form->tree]]
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

(defn raise-sexp [loc]
  (-> loc z/up (z/replace (z/node loc))))

(defn splice-sexp [loc]
  (if (coll-node? (z/node loc))
      (let [nodes   (:children (z/node loc))
            new-loc (reduce z/insert-right loc nodes)
            new-loc (nth (iterate z/left new-loc) (count nodes))]
        (z/remove new-loc))
      loc))

;; keybinds

(def default-binds
  {key/ALT        raise-sexp
   key/BACKSPACE  remove-node
   key/DOWN       z/down
   key/ENTER      (comp insert-form z/up)
   key/LEFT       z/left
   key/META       splice-sexp
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
           {:target (.getElementById js/document "flense-parent")
            :tx-listen #(when (= (:tag %1) :insert-coll)
                          (swap! app-state z/down))})
  (.addEventListener js/window "keydown" (partial handle-key default-binds)))

(init)
