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

(def commands
  {; navigation ---------------------------------------------------------------
   :nav/backward           z/backward
   :nav/down               z/down
   :nav/forward            z/forward
   :nav/left               z/left-or-wrap
   :nav/right              z/right-or-wrap
   :nav/up                 z/up

   ; insertion & deletion -----------------------------------------------------
   :edit/delete            e/delete-sexp
   :edit/insert-left       #(z/insert-left  % p/placeholder)
   :edit/insert-outside    (comp #(z/insert-right % p/placeholder) z/up)
   :edit/insert-right      #(z/insert-right % p/placeholder)

   ; paredit ------------------------------------------------------------------
   :par/barf-left          e/barf-left
   :par/barf-right         e/barf-right
   :par/join-left          e/join-left
   :par/join-right         e/join-right
   :par/make-curly         #(z/edit % (partial e/set-sexp-type :map))
   :par/make-round         #(z/edit % (partial e/set-sexp-type :seq))
   :par/make-square        #(z/edit % (partial e/set-sexp-type :vec))
   :par/raise              e/raise-sexp
   :par/slurp-left         e/slurp-left
   :par/slurp-right        e/slurp-right
   :par/splice             e/splice-sexp
   :par/split-left         e/split-left
   :par/split-right        e/split-right
   :par/swap-left          e/swap-left
   :par/swap-right         e/swap-right

   ; semantic editing ---------------------------------------------------------
   :clj/expand-template    e/expand-sexp
   :clj/toggle-dispatch    e/toggle-dispatch

   ; search & replace ---------------------------------------------------------
   :find/next-placeholder  e/find-placeholder-right
   :find/prev-placeholder  e/find-placeholder-left

   ; clipboard ----------------------------------------------------------------
   :clip/copy              e/copy-sexp!
   :clip/cut               (comp e/delete-sexp e/copy-sexp!)
   :clip/paste             e/paste-sexp

   ; history ------------------------------------------------------------------
   :hist/redo              hist/redo
   :hist/undo              hist/undo})

(def ^:dynamic *keybinds*)

(defn- handle-key [tx-chan ev]
  (let [ks (key-data ev)]
    (when (= ks #{:CTRL :X})
      (.. js/document (getElementById "command-bar") focus))
    (when-let [exec-bind (-> ks *keybinds* commands)]
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
  (set! *keybinds* (p/load-config "resources/config/keymap.edn"))
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
