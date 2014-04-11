(ns flense.core
  (:require [flense.keys :as keys]
            [flense.render :as render]
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
    {:node form :type (classify form)}
    (when (coll? form) {:children (mapv form->tree form)})))

(def app-state
  (atom
    {:lines [(form->tree '(fn greet [name] (str "Hello, " name "!")))]
     :line 0
     :path []}))

(defn- sibling* [path n]
  (when (seq path) (conj (pop path) n)))

(defn- down* [path]
  (conj path 0))

(defn- left* [path]
  (when (seq path)
    (let [n (peek path)]
      (when (pos? n) (sibling* path (dec n))))))

(defn- right* [path]
  (when (seq path)
    (sibling* path (inc (peek path)))))

(defn- up* [path]
  (when (seq path) (pop path)))

(defn- node-at [path tree]
  (get-in tree (concat [:children] (interpose :children path))))

(defn- update-path [tree path f & args]
  (let [update (partial update-in tree (interpose :children path) f)]
    (apply update args)))

(defn- move-to [new-path {:keys [line path] :as state}]
  (-> state
      (assoc :path new-path)
      (update-in [:lines line] update-path path dissoc :selected?)
      (update-in [:lines line] update-path new-path assoc :selected? true)))

(defn go-down [{:keys [line lines path] :as state}]
  (let [new-path (down* path)]
    (if (node-at new-path (lines line))
        (move-to new-path state)
        state)))

(defn go-up [{:keys [path] :as state}]
  (if-let [new-path (up* path)]
    (move-to new-path state)
    state))

(defn go-left [{:keys [line lines path] :as state}]
  (if (seq path)
      (let [new-path
            (or (left* path)
                (sibling* path (-> path (node-at state) :children count dec)))]
        (move-to new-path state))
      (let [new-line (dec line)
            new-line (if (>= new-line 0) new-line (dec (count lines)))]
        (-> state
            (assoc :line new-line)
            (update-in [:lines line] dissoc :selected?)
            (update-in [:lines new-line] assoc :selected? true)))))

(defn go-right [{:keys [line lines path] :as state}]
  (if (seq path)
      (let [new-path (or (right* path) (sibling* path 0))]
        (move-to new-path state))
      (let [new-line (inc line)
            new-line (if (>= new-line (count lines)) 0 new-line)]
        (-> state
            (assoc :line new-line)
            (update-in [:lines line] dissoc :selected?)
            (update-in [:lines new-line] assoc :selected? true)))))

;(defn insert-coll [coll loc]
;  (-> loc
;      (zip/insert-right coll)
;      go-right
;      (zip/insert-child '...)
;      go-down))

;(defn insert-token [loc]
;  (-> loc
;      (zip/insert-right '...)
;      go-right))

;(defn remove-node [loc]
;  (if (zip/up loc) (zip/remove loc) loc))

(def templates
  {'def  '(def ... ...)
   'defn '(defn ... [...] ...)
   'fn   '(fn [...] ...)
   'let  '(let [... ...] ...)})

;(defn expand-node [loc]
;  (if-let [template (templates (zip/node loc))]
;    (zip/replace loc template)
;    loc))

;; keybinds

(def default-binds
  { ;; simple navigation commands
   :DOWN  go-down
   :LEFT  go-left
   :RIGHT go-right
   :UP    go-up
    ;; structure editing: insertion
   ;#{:SHIFT :NUM_9} (partial insert-coll ())
   ;#{:SHIFT :LBRAK} (partial insert-coll {})
   ;:LBRAK           (partial insert-coll [])
   ;:SPACE           insert-token
    ;; structure editing: other
   ;:DEL  remove-node
   ;:TAB  expand-node
   })

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
  (let [state @app-state]
    ;; HACK: autoselect the first top-level form at startup
    (reset! app-state (move-to (:path state) state)))
  (om/root render/root-view app-state {:target (.-body js/document)})
  (keys/trap-modal-keys! modal-keys)
  (.addEventListener js/window "keydown" (partial handle-key default-binds)))

(init)
