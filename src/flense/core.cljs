(ns flense.core
  (:require [flense.keys :as keys]
            [flense.render :as render]
            [flense.zip :as z]
            [om.core :as om]))

(enable-console-print!)

(defn- classify [x]
  (cond (false?   x) :bool
        (true?    x) :bool
        (keyword? x) :keyword
        (map?     x) :map
        (nil?     x) :nil
        (number?  x) :number
        (seq?     x) :seq
        (set?     x) :set
        (string?  x) :string
        (symbol?  x) :symbol
        (vector?  x) :vec))

(defn- form->tree [form]
  (merge
    {:form form :type (classify form)}
    (when (coll? form)
      {:children (mapv form->tree
                       (if (map? form)
                           (interpose (keys form) (vals form))
                           form))})))

(def app-state
  (atom
    (z/->BoundedZipper
      (z/->SimpleZipper [0]
        {:children
         [(form->tree '(fn greet [name] (str "Hello, " name "!")))]}))))

(defn insert-form [form loc]
  (z/down (z/insert-right loc (form->tree form))))

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
    ;; structure editing: insertion
   #{:SHIFT :NUM_9} (partial insert-form '(...))
   #{:SHIFT :LBRAK} (partial insert-form '{... ...})
   :LBRAK           (partial insert-form '[...])
   :SPACE           (partial insert-form '...)
    ;; structure editing: other
   ;:DEL  remove-node
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
  (om/root render/root-view app-state
           {:target (.getElementById js/document "flense-parent")})
  (keys/trap-modal-keys! modal-keys)
  (.addEventListener js/window "keydown" (partial handle-key default-binds)))

(init)
