(ns flense.core
  (:require [clojure.string :as string]
            [flense.keys :as keys]
            [flense.ranges :as ranges]))
(comment
(enable-console-print!)

(def $ js/$)

(defn $empty? [$elem]
  (= (.-length $elem) 0))

;; selection management

(def selected (atom ($ ".selected")))

(defn enable-editing! [$elem]
  (when (.hasClass $elem "token")
    (.attr $elem "contenteditable" true)
    (let [elem (.get $elem 0)]
      (.focus elem)
      (ranges/select-contents! elem))))

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
        (when-not ($empty? $items)
          (select! (.first $items)))))))

(defn go-up! []
  (let [$selected @selected
        $parent   (.parent $selected)]
    (when-not (.hasClass $parent "flense")
      (select! $parent))))

(defn go-left! []
  (let [$selected @selected
        $prev     (.prev $selected)]
    (if ($empty? $prev)
        (let [$last (-> $selected (.siblings) (.last))]
          (when-not ($empty? $last)
            (select! $last)))
        (select! $prev))))

(defn go-right! []
  (let [$selected @selected
        $next     (.next $selected)]
    (if ($empty? $next)
        (let [$first (-> $selected (.siblings) (.first))]
          (when-not ($empty? $first)
            (select! $first)))
        (select! $next))))

;; structure editing commands

(defn- render-token [text]
  (str "<span class=\"token\">" text "</span>"))

(def ^:private render-placeholder
  (partial render-token "..."))

(defn- render-coll
  ([type] (render-coll type [(render-placeholder)]))
  ([type items]
    (str "<div class=\"coll " (name type) "\">"
         (string/join items)
         "</div>")))

(defn open-coll! [type]
  (let [$selected @selected]
    (.after $selected (render-coll type))
    (go-right!)
    (go-down!)))

(defn break-token! []
  (let [$selected @selected]
    (.after $selected (render-placeholder))
    (go-right!)))

(defn delete-selected! []
  (let [$deleted @selected]
    (if (= (.-length (.siblings $deleted)) 0)
        (go-up!)
        (go-left!))
    (.remove $deleted)))

(defn replace-selected! [$replacement]
  (let [$replaced @selected]
    (.replaceWith $replaced $replacement)
    (select! $replacement)))

(defn- emulate-backspace!
  "Emulates the native backspace text editing command, removing the last
   character of the text in `$elem`."
  [$elem]
  (let [text (.text $elem)]
    (.text $elem (subs text 0 (dec (count text))))
    ;; move cursor to end of elem content
    (ranges/select-range! (doto (ranges/selected-range)
                            (.selectNodeContents (.get $elem 0))
                            (.collapse false)))))

(defn- exec-delete! []
  (let [$selected @selected]
       (if (and (.hasClass $selected "token")
                (not (#{"" "..."} (.text $selected)))
                (empty? (.toString (ranges/selected-range))))
           (emulate-backspace! $selected)
           (delete-selected!))))

;; templates and autocompletion

(defn- classify-coll [coll]
  (cond (map? coll)    :map
        (seq? coll)    :seq
        (set? coll)    :set
        (vector? coll) :vec))

(def ^:private token? (complement coll?))

(defn- render-template [template]
  (if (token? template)
      (render-token (pr-str template))
      (render-coll (classify-coll template)
                   (map render-template template))))

(def default-templates
  '{"def"  (def ... ...)
    "defn" (defn ... "" [...] ...)
    "fn"   (fn [...] ...)
    "let"  (let [... ...] ...)
    "loop" (loop [... ...] ...)
    "try"  (try ... (catch Exception e ...))})

(defn- complete [text completions]
  text) ; TODO this is a no-op for now

(defn expand-selected! [templates completions]
  (let [$expanded @selected
        text      (.text $expanded)]
    (when (.hasClass $expanded "token")
      (if-let [template (get templates text)]
        (replace-selected! ($ (render-template template)))
        (.text $expanded (complete text completions))))))

;; keybinds

(def default-binds
  { ;; structural editing commands
   :DEL   exec-delete!
   :LBRAK (partial open-coll! :vec)
   :SPACE break-token!
   :TAB   (partial expand-selected! default-templates {})
   #{:SHIFT :NUM_9} (partial open-coll! :seq)
   #{:SHIFT :LBRAK} (partial open-coll! :map)
    ;; simple navigation commands
   :DOWN  go-down!
   :LEFT  go-left!
   :RIGHT go-right!
   :UP    go-up!})

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
    (when-let [exec-bind! (get keybinds pressed)]
      (.preventDefault ev)
      (exec-bind! ev))))

;; application setup and wiring

(defn init []
  (keys/trap-modal-keys! modal-keys)
  (.keydown ($ js/window) (partial handle-key default-binds)))

(init)
)