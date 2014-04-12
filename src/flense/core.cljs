(ns flense.core
  (:require [flense.keys :as keys]
            [flense.render :as render]
            [flense.util :refer [insert]]
            [flense.zip
             :refer [assoc-path down* get-path insert-right left* right*
                     sibling* update-path up*]]
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
    (when (coll? form)
      {:children (mapv form->tree
                       (if (map? form)
                           (interpose (keys form) (vals form))
                           form))})))

(def app-state
  (atom
    {:lines [(form->tree '(fn greet [name] (str "Hello, " name "!")))]
     :line 0
     :path []}))

(defn- move-to-path [{line :line old-path :path :as state} new-path]
  (let [tree
        (-> ((:lines state) line)
            (update-path old-path dissoc :selected?)
            (update-path new-path assoc :selected? true))]
    (-> state
        (assoc :path new-path)
        (assoc-in [:lines line] tree))))

(defn go-down [{:keys [line lines path] :as state}]
  (let [new-path (down* path)]
    (if (get-path (lines line) new-path)
        (move-to-path state new-path)
        state)))

(defn go-up [{:keys [path] :as state}]
  (if-let [new-path (up* path)]
    (move-to-path state new-path)
    state))

(defn go-left [{:keys [line lines path] :as state}]
  (if (seq path)
      (let [new-path (or (left* path)
                         (sibling* path (-> (get-path (lines line) (up* path))
                                            :children count dec)))]
        (move-to-path state new-path))
      (let [new-line (dec line)
            new-line (if (>= new-line 0) new-line (dec (count lines)))]
        (-> state
            (assoc :line new-line)
            (update-in [:lines line] dissoc :selected?)
            (update-in [:lines new-line] assoc :selected? true)))))

(defn go-right [{:keys [line lines path] :as state}]
  (if (seq path)
      (let [new-path (right* path)
            new-path (if (get-path (lines line) new-path)
                         new-path
                         (sibling* path 0))]
        (move-to-path state new-path))
      (let [new-line (inc line)
            new-line (if (>= new-line (count lines)) 0 new-line)]
        (-> state
            (assoc :line new-line)
            (update-in [:lines line] dissoc :selected?)
            (update-in [:lines new-line] assoc :selected? true)))))

(defn insert-form [form {:keys [line path] :as state}]
  (let [inserted (form->tree form)
        new-state
        (if (seq path)
            (update-in state [:lines line] insert-right path inserted)
            (update-in state [:lines] insert (inc line) inserted))]
    (-> new-state go-right go-down)))

;(defn remove-node [loc]
;  (if (zip/up loc) (zip/remove loc) loc))

(def templates
  {'def  '(def ... ...)
   'defn '(defn ... [...] ...)
   'fn   '(fn [...] ...)
   'let  '(let [... ...] ...)})

(defn expand-node [{:keys [line lines path] :as state}]
  (let [node (:node (get-path (lines line) path))]
    (if-let [template (templates node)]
      (update-in state [:lines line] assoc-path path (form->tree template))
      state)))

;; keybinds

(def default-binds
  { ;; simple navigation commands
   :DOWN  go-down
   :LEFT  go-left
   :RIGHT go-right
   :UP    go-up
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
  (swap! app-state go-right) ; select first top-level form
  (om/root render/root-view app-state {:target (.-body js/document)})
  (keys/trap-modal-keys! modal-keys)
  (.addEventListener js/window "keydown" (partial handle-key default-binds)))

(init)
