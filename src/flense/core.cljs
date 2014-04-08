(ns flense.core
  (:require [clojure.string :as string]
            [flense.keys :as keys]
            [flense.render :as render]
            [reagent.core :as reagent]))

(enable-console-print!)

(def app-state
  (reagent/atom {:tree   ['(fn greet [name] (str "Hello, " name "!"))]
                 :cursor [0]}))

(extend-protocol ILookup
  List
  (-lookup [this k]
    (get (vec this) k))
  (-lookup [this k not-found]
    (or (get (vec this) k) not-found)))

(defn node-at [path {:keys [tree]}]
  (get-in tree path))

(defn selected-node [{:keys [cursor] :as state}]
  (node-at cursor state))

(defn parent-node [{:keys [cursor] :as state}]
  (node-at (pop cursor) state))

(defn go-down [state]
  (let [node (selected-node state)]
    (if (and (coll? node) (seq node))
        (update-in state [:cursor] conj 0)
        state)))

(defn go-up [{:keys [cursor] :as state}]
  (if (> (count cursor) 1)
      (update-in state [:cursor] pop)
      state))

(defn go-right [{:keys [cursor] :as state}]
  (let [new-cursor (conj (pop cursor) (inc (peek cursor)))]
    (if (node-at new-cursor state)
        (assoc state :cursor new-cursor)
        (let [new-cursor (conj (pop cursor) 0)]
          (if (node-at new-cursor state)
              (assoc state :cursor new-cursor)
              state)))))

(defn go-left [{:keys [cursor] :as state}]
  (let [new-cursor (conj (pop cursor) (dec (peek cursor)))]
    (if (node-at new-cursor state)
        (assoc state :cursor new-cursor)
        (let [new-cursor (conj (pop cursor) (dec (count (parent-node state))))]
          (if (node-at new-cursor state)
              (assoc state :cursor new-cursor)
              state)))))

;; keybinds

(def default-binds
  { ;; simple navigation commands
   :DOWN  go-down
   :LEFT  go-left
   :RIGHT go-right
   :UP    go-up})

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
  (reagent/render-component [(partial render/root app-state)]
                            (.-body js/document))
  (keys/trap-modal-keys! modal-keys)
  (.addEventListener js/window "keydown" (partial handle-key default-binds)))

(init)
