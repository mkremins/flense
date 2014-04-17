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

(defn slurp-left [loc]
  (let [coll-loc (if (coll-node? (z/node loc)) loc (z/up loc))
        left-loc (z/left coll-loc)]
    (cond
      ;; if we wrapped all the way to the right, there's nothing to slurp
      (= left-loc (z/rightmost coll-loc)) loc
      ;; if we're all the way to the left, z/remove takes us up a level
      (= left-loc (z/leftmost coll-loc))
      (-> left-loc
          z/remove z/down z/down z/leftmost
          (z/insert-left (z/node left-loc)))
      ;; if we're not, then z/remove only takes us to the left
      :else
      (-> left-loc
          z/remove z/right z/down z/leftmost
          (z/insert-left (z/node left-loc))))))

(defn slurp-right [loc]
  (let [coll-loc  (if (coll-node? (z/node loc)) loc (z/up loc))
        right-loc (z/right coll-loc)]
    (if (= right-loc (z/leftmost coll-loc))
        loc
        (-> right-loc
            z/remove z/down z/rightmost
            (z/insert-right (z/node right-loc))))))

(defn raise-sexp [loc]
  (-> loc z/up (z/replace (z/node loc))))

(defn splice-sexp [loc]
  (if (coll-node? (z/node loc))
      (let [nodes   (:children (z/node loc))
            new-loc (reduce z/insert-right loc nodes)
            new-loc (nth (iterate z/left new-loc) (count nodes))]
        (-> new-loc z/remove z/right))
      loc))

;; keybinds

(def default-binds
  {key/BACKSPACE  remove-node
   key/DOWN       z/down
   key/ENTER      (comp insert-form z/up)
   key/LEFT       z/left
   key/RIGHT      z/right
   key/SPACE      insert-form
   key/TAB        expand-node
   key/UP         z/up})

(def ctrl-binds
  {key/R     raise-sexp
   key/NINE  slurp-left
   key/ZERO  slurp-right
   key/S     splice-sexp})

(defn handle-key [ev]
  (let [keybinds (if (.-ctrlKey ev) ctrl-binds default-binds)]
    (when-let [exec-bind (get keybinds (.-keyCode ev))]
      (.preventDefault ev)
      (swap! app-state exec-bind))))

;; application setup and wiring

(defn init []
  (om/root render/root-view app-state
           {:target (.getElementById js/document "flense-parent")
            :tx-listen #(when (= (:tag %1) :insert-coll)
                          (swap! app-state z/down))})
  (.addEventListener js/window "keydown" handle-key))

(init)
