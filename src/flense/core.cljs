(ns flense.core
  (:require [clojure.zip :as zip]
            [flense.keys :as keys]
            [flense.render :as render]
            [reagent.core :as reagent]))

(enable-console-print!)

(defn forms-zip [root]
  (zip/zipper coll?
              (fn [parent]
                (if (map? parent)
                    (interleave (keys parent) (vals parent))
                    parent))
              (fn [parent children]
                (cond (map? parent) (apply hash-map children)
                      (seq? parent) (apply list children)
                      (set? parent) (set children)
                      (vector? parent) (vec children)))
              root))

(def app-state
  (reagent/atom (forms-zip ['(fn greet [name] (str "Hello, " name "!"))])))

(defn go-down [loc]
  (or (zip/down loc) loc))

(defn go-up [loc]
  (or (zip/up loc) loc))

(defn go-left [loc]
  (or (zip/left loc) (zip/rightmost loc)))

(defn go-right [loc]
  (or (zip/right loc) (zip/leftmost loc)))

(defn insert-coll [coll loc]
  (-> loc
      (zip/insert-right coll)
      go-right
      (zip/insert-child '...)
      go-down))

(defn insert-token [loc]
  (-> loc
      (zip/insert-right '...)
      go-right))

(defn remove-node [loc]
  (if (zip/up loc) (zip/remove loc) loc))

(def templates
  {'def  '(def ... ...)
   'defn '(defn ... [...] ...)
   'fn   '(fn [...] ...)
   'let  '(let [... ...] ...)})

(defn expand-node [loc]
  (if-let [template (templates (zip/node loc))]
    (zip/replace loc template)
    loc))

;; keybinds

(def default-binds
  { ;; simple navigation commands
   :DOWN  go-down
   :LEFT  go-left
   :RIGHT go-right
   :UP    go-up
    ;; structure editing: insertion
   #{:SHIFT :NUM_9} (partial insert-coll ())
   #{:SHIFT :LBRAK} (partial insert-coll {})
   :LBRAK           (partial insert-coll [])
   :SPACE           insert-token
    ;; structure editing: other
   :DEL  remove-node
   :TAB  expand-node})

(def modal-keys #{:ALT :CTRL :SHIFT})

(defn handle-key [keybinds ev]
  (let [pressed
        (->> modal-keys
             (filter keys/held?)
             (reduce conj #{(keys/ev->key ev)}))
        pressed
        (if (= (count pressed) 1)
            (first pressed)
            pressed)]
    (when-let [exec-bind (get keybinds pressed)]
      (.preventDefault ev)
      (swap! app-state exec-bind))))

;; application setup and wiring

(defn init []
  (reagent/render-component [render/root app-state] (.-body js/document))
  (keys/trap-modal-keys! modal-keys)
  (.addEventListener js/window "keydown" (partial handle-key default-binds))
  (swap! app-state go-down)) ; select the first top-level form at startup

(init)
