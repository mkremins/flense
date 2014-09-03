(ns flense.app
  (:require [cljs.core.async :as async :refer [<!]]
            [cljs.reader :as rdr]
            [flense.edit :refer [action actions]]
            [flense.edit.history :as hist]
            flense.edit.clipboard
            flense.edit.clojure
            flense.edit.movement
            flense.edit.paredit
            [flense.keymap :as keymap]
            [flense.model :as model]
            [flense.ui.cli :refer [cli-view]]
            [flense.ui.editor :refer [editor-view]]
            flense.ui.editor.layout
            [flense.ui.error :refer [error-bar-view]]
            [flense.util.dom :as udom]
            fs
            [om.core :as om])
  (:require-macros [cljs.core.async.macros :refer [go-loop]]))

(enable-console-print!)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; top-level state setup and management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private app-state
  (atom
   {:path [0]
    :tree {:children
           [(model/form->tree '(fn greet [name] (str "Hello, " name "!")))]}}))

(def ^:private edit-chan (async/chan))
(def ^:private error-chan (async/chan))

(defn raise!
  "Display error message `mparts` to the user in the popover error bar."
  [& mparts]
  (async/put! error-chan (apply str mparts)))

(defn open!
  "Load the source file at `fpath` and open the loaded document, discarding any
   changes made to the previously active document."
  [fpath]
  (reset! app-state
    {:path [0]
     :tree {:children
            (->> (fs/slurp fpath) model/string->forms (mapv model/form->tree))}}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; text commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti handle-command (fn [command & _] command))

(defmethod handle-command :default [command & _]
  (raise! "Invalid command \"" command \"))

(defmethod handle-command "exec" [_ & args]
  (if-let [name (first args)]
    (if-let [action (-> name rdr/read-string (@actions))]
      (async/put! edit-chan action)
      (raise! "Invalid action \"" name \"))
    (raise! "Must specify an action to execute")))

(defmethod handle-command "open" [_ & args]
  (if-let [fpath (first args)]
    (open! fpath)
    (raise! "Must specify a filepath to open")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; application setup and wiring
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(action :flense/text-command :edit identity) ; dummy action to trap ctrl+x keybind

(defn- handle-key [ev]
  (when-let [action (keymap/bound-action ev)]
    (if (= (:name action) :flense/text-command)
      (.. js/document (getElementById "cli") focus)
      (do (.preventDefault ev)
          (async/put! edit-chan action)))))

(defn- handle-tx [{:keys [new-state tag] :or {tag #{}}}]
  (when-not (tag :history)
    (hist/push-state! new-state)))

(defn- propagate-keypress? [ev form]
  (if (model/stringlike? form)
    ;; prevent all keybinds except those that end editing
    (-> ev keymap/bound-action :name
        #{:flense/text-command :move/up :paredit/insert-outside})
    ;; prevent delete keybind unless text fully selected
    (or (not= (:name (keymap/bound-action ev)) :flense/remove)
        (udom/fully-selected? (.-target ev)))))

(defn init []
  (set! keymap/*bindings*
    (rdr/read-string (fs/slurp "resources/config/keymap.edn")))
  (let [command-chan (async/chan)]
    (hist/push-state! @app-state)
    (om/root editor-view app-state
             {:target (.getElementById js/document "editor-parent")
              :opts {:edit-chan edit-chan
                     :propagate-keypress? propagate-keypress?}
              :tx-listen handle-tx})
    (om/root cli-view nil
             {:target (.getElementById js/document "cli-parent")
              :shared {:command-chan command-chan}})
    (om/root error-bar-view nil
             {:target (.getElementById js/document "error-bar-parent")
              :shared {:error-chan error-chan}})
    (go-loop []
      (let [[command & args] (<! command-chan)]
        (apply handle-command command args))
      (recur))
    (.addEventListener js/window "keydown" handle-key)))

(init)
