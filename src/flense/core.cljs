(ns flense.core
  (:require [cljs.core.async :as async]
            [flense.edit :as e]
            [flense.history :as hist]
            [flense.parse :as p]
            [flense.ui :as ui]
            [flense.util :refer [maybe]]
            [flense.zip :as z]
            [fs]
            [goog.events.KeyCodes :as key]
            [om.core :as om])
  (:require-macros [cljs.core.async.macros :refer [go-loop]]))

(enable-console-print!)

(def app-state
  (atom
   {:path [0]
    :tree {:children
           [(p/form->tree '(fn greet [name] (str "Hello, " name "!")))]}}))

(defn load! [fpath]
  (reset! app-state
   {:path [0]
    :tree {:children
           (->> (fs/slurp fpath)
                p/string->forms
                (mapv p/form->tree))}}))

;; keybinds

(def default-binds
  {key/BACKSPACE e/delete-sexp
   key/DOWN   z/down
   key/ENTER  (comp #(e/insert-right % p/placeholder) z/up)
   key/LEFT   z/left-or-wrap
   key/RIGHT  z/right-or-wrap
   key/SPACE  #(e/insert-right % p/placeholder)
   key/TAB    e/expand-sexp
   key/UP     z/up})

(def ctrl-binds
  {key/OPEN_SQUARE_BRACKET
   #(z/edit % (partial e/set-sexp-type :vec))})

(def ctrl-shift-binds
  {key/NINE
   #(z/edit % (partial e/set-sexp-type :seq))
   key/OPEN_SQUARE_BRACKET
   #(z/edit % (partial e/set-sexp-type :map))})

(def meta-binds
  {key/C  e/copy-sexp!
   key/V  e/paste-sexp
   key/X  (comp e/delete-sexp e/copy-sexp!)
   key/Y  hist/redo
   key/Z  hist/undo})

(def meta-ctrl-binds
  {key/A      e/join-left
   key/K      e/swap-left
   key/L      e/swap-right
   key/LEFT   e/barf-left
   key/NINE   e/split-left
   key/RIGHT  e/barf-right
   key/S      e/join-right
   key/UP     e/splice-sexp
   key/ZERO   e/split-right})

(def meta-shift-binds
  {key/K      e/find-placeholder-left
   key/L      e/find-placeholder-right
   key/LEFT   e/slurp-left
   key/RIGHT  e/slurp-right
   key/UP     e/raise-sexp})

(def shift-binds
  {key/LEFT   z/backward
   key/RIGHT  z/forward
   key/SPACE  #(e/insert-left % p/placeholder)
   key/THREE  e/toggle-dispatch})

(defn handle-key [ev]
  (when (and (.-ctrlKey ev) (= (.-keyCode ev) key/X))
    (.. js/document (getElementById "command-bar") focus))
  (let [keybinds
        (cond (and (.-ctrlKey ev) (.-shiftKey ev)) ctrl-shift-binds
              (and (.-metaKey ev) (.-ctrlKey ev))  meta-ctrl-binds
              (and (.-metaKey ev) (.-shiftKey ev)) meta-shift-binds
              (.-ctrlKey ev)  ctrl-binds
              (.-metaKey ev)  meta-binds
              (.-shiftKey ev) shift-binds
              :else default-binds)]
    (when-let [exec-bind (get keybinds (.-keyCode ev))]
      (.preventDefault ev)
      (om/transact! ui/*root-cursor* [] (partial maybe exec-bind)
                    (when (#{hist/redo hist/undo} exec-bind) ::hist/ignore)))))

;; text commands

(defn- handle-command [command & args]
  (when (and (= command "load") (first args))
    (load! (first args))))

;; application setup and wiring

(defn- handle-tx [{:keys [new-state tag]}]
  (when (= tag :wrap-coll)
    (om/transact! ui/*root-cursor* [] z/down ::hist/ignore))
  (when-not (= tag ::hist/ignore)
    (hist/push-state! new-state)))

(defn init []
  (let [command-chan (async/chan)]
    (hist/push-state! @app-state)
    (om/root ui/root-view app-state
             {:target (.getElementById js/document "flense-parent")
              :tx-listen handle-tx})
    (om/root ui/command-bar-view nil
             {:target (.getElementById js/document "command-bar-parent")
              :shared {:command-chan command-chan}})
    (go-loop []
      (let [[command & args] (<! command-chan)]
        (apply handle-command command args))
      (recur))
    (.addEventListener js/window "keydown" handle-key)))

(init)
