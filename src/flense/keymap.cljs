(ns flense.keymap
  (:require [flense.edit :refer [actions]]
            [phalanges.core :as phalanges]))

(def ^:dynamic *bindings*)

(defn bound-action [ev]
  (-> ev phalanges/key-set *bindings* (@actions)))
