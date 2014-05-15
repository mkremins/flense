(ns flense.app
  (:require [cljs.core.async :as async :refer [<!]]
            [flense.commands :refer [commands]]
            [flense.history :as hist]
            [flense.keyboard :refer [key-data]]
            [flense.parse :as p]
            [flense.ui.cli :refer [cli-view]]
            [flense.ui.editor :refer [editor-view]]
            [flense.ui.error :refer [error-bar-view]]
            [flense.util :refer [maybe]]
            [flense.zip :as z]
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
           [(p/form->tree '(fn greet [name] (str "Hello, " name "!")))]}}))

(def ^:dynamic ^:private *error-chan*)
(def ^:dynamic ^:private *tx-chan*)

(defn raise!
  "Display error message `message` to the user in the popover error bar."
  [message]
  (async/put! *error-chan* message))

(defn exec!
  "Execute command `f` on the active document, optionally tagging the resulting
   transaction with `tag`."
  ([f] (exec! f nil))
  ([f tag] (async/put! *tx-chan* {:fn (partial maybe f) :tag tag})))

(defn open!
  "Load the source file at `fpath` and open the loaded document, discarding any
   changes made to the previously active document."
  [fpath]
  (reset! app-state (p/load-source fpath)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keybinds
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:dynamic *keybinds*)

(defn- handle-key [ev]
  (let [ks (key-data ev)]
    (when (= ks #{:CTRL :X})
      (.. js/document (getElementById "cli") focus))
    (when-let [keybind (-> ks *keybinds* commands)]
      (.preventDefault ev)
      (exec! keybind
       (when (#{hist/redo hist/undo} keybind) ::hist/ignore)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; text commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- handle-command [command & args]
  (condp = command
    "open"
    (if-let [fpath (first args)]
      (open! fpath)
      (raise! "Must specify a filepath to open"))
    nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; application setup and wiring
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- handle-tx [{:keys [new-state tag]}]
  (when-not (= tag ::hist/ignore)
    (hist/push-state! new-state)))

(defn init []
  (set! *keybinds* (p/load-config "resources/config/keymap.edn"))
  (set! *error-chan* (async/chan))
  (set! *tx-chan* (async/chan))
  (let [command-chan (async/chan)]
    (hist/push-state! @app-state)
    (om/root editor-view app-state
             {:target (.getElementById js/document "editor-parent")
              :shared {:tx-chan *tx-chan*}
              :tx-listen handle-tx})
    (om/root cli-view nil
             {:target (.getElementById js/document "cli-parent")
              :shared {:command-chan command-chan}})
    (om/root error-bar-view nil
             {:target (.getElementById js/document "error-bar-parent")
              :shared {:error-chan *error-chan*}})
    (go-loop []
      (let [[command & args] (<! command-chan)]
        (apply handle-command command args))
      (recur))
    (.addEventListener js/window "keydown" handle-key)))

(init)
