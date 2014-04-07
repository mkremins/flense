(ns flense.core
  (:use [flense.keys :only [ev->key held? trap-modal-keys!]]))

(enable-console-print!)

(def $ js/$)

;; selection management

(def selected (atom ($ ".selected")))

(defn- select-text! [elem]
  (let [text-range     (.createRange js/document)
        text-selection (.getSelection js/window)]
    (.selectNodeContents text-range elem)
    (.removeAllRanges text-selection)
    (.addRange text-selection text-range)))

(defn enable-editing! [$elem]
  (when (.hasClass $elem "token")
    (.attr $elem "contenteditable" true)
    (let [elem (.get $elem 0)]
      (.focus elem)
      (select-text! elem))))

(defn disable-editing! [$elem]
  (when (.hasClass $elem "token")
    (.removeAttr $elem "contenteditable")
    (.blur (.get $elem 0))))

(defn select! [$elem]
  (let [$prev @selected]
    (disable-editing! $prev)
    (when (and (.hasClass $prev "token") (#{"" "..."} (.text $prev)))
      (.remove $prev))
    (.removeClass $prev "selected"))
  (reset! selected $elem)
  (.addClass $elem "selected")
  (when (.hasClass $elem "token")
    (enable-editing! $elem)))

;; zipper navigation commands

(defn go-down! []
  (let [$selected @selected]
    (when (.hasClass $selected "coll")
      (let [$items (.children $selected)]
        (when-not (= (.-length $items) 0)
          (select! (.first $items)))))))

(defn go-up! []
  (let [$selected @selected
        $parent   (.parent $selected)]
    (when-not (.hasClass $parent "flense")
      (select! $parent))))

(defn go-left! []
  (let [$selected @selected
        $prev     (.prev $selected)]
    (if (= (.-length $prev) 0)
        (let [$last (-> $selected (.siblings) (.last))]
          (when-not (= (.-length $last) 0)
            (select! $last)))
        (select! $prev))))

(defn go-right! []
  (let [$selected @selected
        $next     (.next $selected)]
    (if (= (.-length $next) 0)
        (let [$first (-> $selected (.siblings) (.first))]
          (when-not (= (.-length $first) 0)
            (select! $first)))
        (select! $next))))

;; structure editing commands

(defn- render-token []
  "<span class=\"token\">...</span>")

(defn- render-coll [type]
  (str "<div class=\"coll " (name type) "\">"
       (render-token)
       "</div>"))

(defn open-coll! [type]
  (let [$selected @selected]
    (.after $selected (render-coll type))
    (go-right!)
    (go-down!)))

(defn break-token! []
  (let [$selected @selected]
    (.after $selected (render-token))
    (go-right!)))

(defn delete-selected! []
  (let [$deleted @selected]
    (if (= (.-length (.siblings $deleted)) 0)
        (go-up!)
        (go-left!))
    (.remove $deleted)))

;; keybinds

(def default-binds
  { ;; structural editing commands
   :DEL             delete-selected!
   :LBRAK           (partial open-coll! :vec)
   :SPACE           break-token!
   #{:SHIFT :NUM_9} (partial open-coll! :seq)
   #{:SHIFT :LBRAK} (partial open-coll! :map)
    ;; simple navigation commands
   :DOWN  go-down!
   :LEFT  go-left!
   :RIGHT go-right!
   :UP    go-up!})

(def modal-keys #{:ALT :CTRL :SHIFT})

(defn handle-key [ev]
  (let [keys (->> modal-keys (filter held?) (reduce conj #{(ev->key ev)}))
        keys (if (= (count keys) 1) (first keys) keys)]
    (when-let [exec-bind! (get default-binds keys)]
      (.preventDefault ev)
      (exec-bind! ev))))

;; application setup and wiring

(defn init []
  (trap-modal-keys! modal-keys)
  (.keydown ($ js/window) handle-key))

(init)
