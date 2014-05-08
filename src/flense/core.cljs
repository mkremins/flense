(ns flense.core
  (:require [cljs.core.async :as async :refer [<! put!]]
            [flense.edit :as e]
            [flense.history :as hist]
            [flense.keyboard :refer [key-data]]
            [flense.parse :as p]
            [flense.ui :as ui]
            [flense.util :refer [maybe]]
            [flense.zip :as z]
            [fs]
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

(defn load! [fpath]
  (reset! app-state
   {:path [0]
    :tree {:children
           (->> (fs/slurp fpath)
                p/string->forms
                (mapv p/form->tree))}}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keybinds
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def keybinds
  {; naked --------------------------------------------------------------------
   #{:BACKSPACE}          e/delete-sexp
   #{:DOWN}               z/down
   #{:ENTER}              (comp #(z/insert-right % p/placeholder) z/up)
   #{:LEFT}               z/left-or-wrap
   #{:RIGHT}              z/right-or-wrap
   #{:SPACE}              #(z/insert-right % p/placeholder)
   #{:TAB}                e/expand-sexp
   #{:UP}                 z/up

   ; CTRL, CTRL+SHIFT ---------------------------------------------------------
   #{:CTRL :LBRAK}        #(z/edit % (partial e/set-sexp-type :vec))
   #{:CTRL :SHIFT :NINE}  #(z/edit % (partial e/set-sexp-type :seq))
   #{:CTRL :SHIFT :LBRAK} #(z/edit % (partial e/set-sexp-type :map))

   ; CMD ----------------------------------------------------------------------
   #{:CMD :C}             e/copy-sexp!
   #{:CMD :V}             e/paste-sexp
   #{:CMD :X}             (comp e/delete-sexp e/copy-sexp!)
   #{:CMD :Y}             hist/redo
   #{:CMD :Z}             hist/undo

   ; CMD+CTRL -----------------------------------------------------------------
   #{:CMD :CTRL :A}       e/join-left
   #{:CMD :CTRL :K}       e/swap-left
   #{:CMD :CTRL :L}       e/swap-right
   #{:CMD :CTRL :LEFT}    e/barf-left
   #{:CMD :CTRL :NINE}    e/split-left
   #{:CMD :CTRL :RIGHT}   e/barf-right
   #{:CMD :CTRL :S}       e/join-right
   #{:CMD :CTRL :UP}      e/splice-sexp
   #{:CMD :CTRL :ZERO}    e/split-right

   ; CMD+SHIFT ----------------------------------------------------------------
   #{:CMD :SHIFT :K}      e/find-placeholder-left
   #{:CMD :SHIFT :L}      e/find-placeholder-right
   #{:CMD :SHIFT :LEFT}   e/slurp-left
   #{:CMD :SHIFT :RIGHT}  e/slurp-right
   #{:CMD :SHIFT :UP}     e/raise-sexp

   ; SHIFT --------------------------------------------------------------------
   #{:SHIFT :LEFT}        z/forward
   #{:SHIFT :RIGHT}       z/backward
   #{:SHIFT :SPACE}       #(z/insert-left % p/placeholder)
   #{:SHIFT :THREE}       e/toggle-dispatch})

(defn- handle-key [tx-chan ev]
  (let [ks (key-data ev)]
    (when (= ks #{:CTRL :X})
      (.. js/document (getElementById "command-bar") focus))
    (when-let [exec-bind (get keybinds ks)]
      (.preventDefault ev)
      (put! tx-chan
       {:fn  (partial maybe exec-bind)
        :tag (when (#{hist/redo hist/undo} exec-bind) ::hist/ignore)}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; text commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- handle-command [command & args]
  (when (and (= command "load") (first args))
    (load! (first args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; application setup and wiring
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- handle-tx [tx-chan {:keys [new-state tag]}]
  (when (= tag :wrap-coll)
    (put! tx-chan {:fn z/down :tag ::hist/ignore}))
  (when-not (= tag ::hist/ignore)
    (hist/push-state! new-state)))

(defn init []
  (let [command-chan (async/chan)
             tx-chan (async/chan)]
    (hist/push-state! @app-state)
    (om/root ui/root-view app-state
             {:target (.getElementById js/document "flense-parent")
              :shared {:tx-chan tx-chan}
              :tx-listen (partial handle-tx tx-chan)})
    (om/root ui/command-bar-view nil
             {:target (.getElementById js/document "command-bar-parent")
              :shared {:command-chan command-chan}})
    (go-loop []
      (let [[command & args] (<! command-chan)]
        (apply handle-command command args))
      (recur))
    (.addEventListener js/window "keydown" (partial handle-key tx-chan))))

(init)
