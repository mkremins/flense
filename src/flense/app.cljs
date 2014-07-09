(ns flense.app
  (:require [cljs.core.async :as async :refer [<!]]
            [flense.edit :refer [actions]]
            [flense.edit.history :as hist]
            flense.edit.clipboard
            flense.edit.clojure
            flense.edit.movement
            flense.edit.paredit
            [flense.keyboard :refer [key-data]]
            [flense.parse :as p]
            [flense.ui.cli :refer [cli-view]]
            [flense.ui.editor :refer [editor-view]]
            [flense.ui.error :refer [error-bar-view]]
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
  (reset! app-state (p/load-source fpath)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keybinds
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:dynamic *keybinds*)

(defn- handle-key [ev]
  (let [ks (key-data ev)]
    (when (= ks #{:CTRL :X})
      (.. js/document (getElementById "cli") focus))
    (when-let [keybind (-> ks *keybinds* (@actions))]
      (.preventDefault ev)
      (async/put! edit-chan keybind))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; text commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- handle-command [command & args]
  (case command
    "exec"
      (if-let [name (first args)]
        (if-let [action (-> name p/parse-keyword (@actions))]
          (async/put! edit-chan action)
          (raise! "Invalid action \"" name \"))
        (raise! "Must specify an action to execute"))
    "open"
      (if-let [fpath (first args)]
        (open! fpath)
        (raise! "Must specify a filepath to open"))
    ;else
      (raise! "Invalid command \"" command \")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; application setup and wiring
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- handle-tx [{:keys [new-state tag] :or {tag #{}}}]
  (when-not (tag :history)
    (hist/push-state! new-state)))

(defn init []
  (set! *keybinds* (p/load-config "resources/config/keymap.edn"))
  (let [command-chan (async/chan)]
    (hist/push-state! @app-state)
    (om/root editor-view app-state
             {:target (.getElementById js/document "editor-parent")
              :shared {:edit-chan edit-chan}
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
