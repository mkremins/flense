(ns flense.commands
   (:require [flense.edit :as e]
             [flense.history :as h]
             [flense.parse :as p]
             [flense.zip :as z]))

(def commands
  {; navigation ---------------------------------------------------------------
   :nav/prev               z/prev
   :nav/down               z/down
   :nav/next               z/next
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
   :par/make-curly         e/make-curly
   :par/make-round         e/make-round
   :par/make-square        e/make-square
   :par/raise              e/raise-sexp
   :par/slurp-left         e/slurp-left
   :par/slurp-right        e/slurp-right
   :par/splice             e/splice-sexp
   :par/split-left         e/split-left
   :par/split-right        e/split-right
   :par/swap-left          e/swap-left
   :par/swap-right         e/swap-right
   :par/wrap-curly         e/wrap-curly
   :par/wrap-quote         e/wrap-quote
   :par/wrap-round         e/wrap-round
   :par/wrap-square        e/wrap-square

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
   :hist/redo              h/redo
   :hist/undo              h/undo})
