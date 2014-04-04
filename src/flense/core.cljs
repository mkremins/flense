(ns flense.core
  (:use [flense.keys :only [ev->key held? trap-modal-keys!]]))

(enable-console-print!)

(def $ js/$)

;; selection management

(def selected (atom ($ ".selected")))

(defn select! [$elem]
  (.removeClass @selected "selected")
  (reset! selected $elem)
  (.addClass $elem "selected"))

(defn exists? [$elem]
  (> (.-length $elem) 0))

;; edit commands

(declare enable-edit-mode! go-down! go-right! go-up!)

(defn insert-token! []
  (let [$selected @selected]
    (.after $selected "<span class=\"token\"></span>")
    (go-right!)
    (enable-edit-mode!)))

(defn delete-selected! []
  (let [$deleted @selected]
    (go-right!)
    (when (= @selected $deleted)
      (go-up!))
    (.remove $deleted)))

;; edit mode management

(def edit-mode? (atom false))

(defn enable-edit-mode! []
  (let [$selected @selected]
    (when (.hasClass $selected "token")
      (reset! edit-mode? true)
      (.attr $selected "contenteditable" true)
      (.focus (.get $selected 0)))
    (when (.hasClass $selected "coll")
      (go-down!)
      (enable-edit-mode!))))

(defn disable-edit-mode! []
  (let [$selected @selected]
    (when (.hasClass $selected "token")
      (reset! edit-mode? false)
      (.removeAttr $selected "contenteditable")
      (.blur (.get $selected 0))
      (when-not (seq (.text $selected))
        (delete-selected!)))))

;; zipper navigation commands

(defn go-down! []
  (let [$selected @selected]
    (when (.hasClass $selected "coll")
      (let [$items (.children $selected ".items")]
        (when (exists? $items)
          (select! (-> $items (.children) (.first))))))))

(defn go-up! []
  (let [$selected @selected]
    (when-not (.hasClass $selected "top")
      (select! (-> $selected (.parent) (.parent))))))

(defn go-left! []
  (let [$selected @selected]
    (when-not (.hasClass $selected "top")
      (let [$prev (.prev $selected)]
        (if (exists? $prev)
            (select! $prev)
            (let [$last (-> $selected (.siblings) (.last))]
              (when (exists? $last)
                (select! $last))))))))

(defn go-right! []
  (let [$selected @selected]
    (when-not (.hasClass $selected "top")
      (let [$next (.next $selected)]
        (if (exists? $next)
            (select! $next)
            (let [$first (-> $selected (.siblings) (.first))]
              (when (exists? $first)
                (select! $first))))))))

(defn walk-down! []
  (loop [$selected @selected]
    (when (and (.hasClass $selected "coll")
               (exists? (.children $selected ".items")))
        (go-down!)
        (recur @selected))))

(defn walk-right! []
  (let [$selected @selected]
    (when-not (.hasClass $selected "top")
      (let [$next (.next $selected)]
        (if (exists? $next)
            (select! $next)
            (do (go-up!)
                (go-right!)
                (walk-down!)))))))

;; positional navigation commands

(defn go-to-child! [n]
  (let [$selected @selected]
    (when (.hasClass $selected "coll")
      (let [$child (-> $selected
                       (.find (str ".items > :nth-child(" (inc n) ")" ))
                       (.first))]
        (when (exists? $child)
          (select! $child))))))

(defn go-to-sibling! [n]
  (let [$sibling (.children (.parent @selected)
                            (str ":nth-child(" (inc n) ")"))]
    (when (exists? $sibling)
      (select! $sibling))))

;; keybinds

(def default-binds
  { ;; tree structure editing commands
   :DEL   #(do (.preventDefault %) (delete-selected!))
   :ENTER #(do (.preventDefault %) (enable-edit-mode!))
   :SPACE #(do (.preventDefault %) (insert-token!))
    ;; standard zipper-walking navigation commands
   :DOWN  go-down!
   :LEFT  go-left!
   :RIGHT go-right!
   :UP    go-up!
    ;; jump to nth child of selected coll
   :NUM_1 (partial go-to-child! 0)
   :NUM_2 (partial go-to-child! 1)
   :NUM_3 (partial go-to-child! 2)
   :NUM_4 (partial go-to-child! 3)
   :NUM_5 (partial go-to-child! 4)
   :NUM_6 (partial go-to-child! 5)
   :NUM_7 (partial go-to-child! 6)
   :NUM_8 (partial go-to-child! 7)
    ;; jump to nth sibling of selected node
   #{:CTRL :NUM_1} (partial go-to-sibling! 0)
   #{:CTRL :NUM_2} (partial go-to-sibling! 1)
   #{:CTRL :NUM_3} (partial go-to-sibling! 2)
   #{:CTRL :NUM_4} (partial go-to-sibling! 3)
   #{:CTRL :NUM_5} (partial go-to-sibling! 4)
   #{:CTRL :NUM_6} (partial go-to-sibling! 5)
   #{:CTRL :NUM_7} (partial go-to-sibling! 6)
   #{:CTRL :NUM_8} (partial go-to-sibling! 7)})

(def edit-mode-binds
  { ;; synonyms for "save changes and stop editing"
   :ENTER #(do (.preventDefault %) (disable-edit-mode!))
   :ESC   #(do (.preventDefault %) (disable-edit-mode!))
   :UP    #(do (.preventDefault %) (disable-edit-mode!))
    ;; mode-specific navigation commands
   :TAB   #(do (.preventDefault %) (walk-right!) (enable-edit-mode!))})

(def modal-keys #{:CTRL})

(defn handle-key [ev]
  (let [binds (if @edit-mode? edit-mode-binds default-binds)
        keys  (->> modal-keys (filter held?) (reduce conj #{(ev->key ev)}))
        keys  (if (= (count keys) 1) (first keys) keys)]
    (when-let [exec-bind! (get binds keys)]
      (exec-bind! ev))))

;; application setup and wiring

(defn init []
  (trap-modal-keys! modal-keys)
  (.keydown ($ js/window) handle-key))

(init)
