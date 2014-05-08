(ns flense.keyboard
  (:require [goog.events.KeyCodes :as k]))

(def key-name
  {k/A :A, k/B :B, k/C :C, k/D :D, k/E :E, k/F :F, k/G :G, k/H :H, k/I :I,
   k/J :J, k/K :K, k/L :L, k/M :M, k/N :N, k/O :O, k/P :P, k/Q :Q, k/R :R,
   k/S :S, k/T :T, k/U :U, k/V :V, k/W :W, k/X :X, k/Y :Y, k/Z :Z,
   k/BACKSPACE :BACKSPACE
   k/DOWN  :DOWN
   k/ENTER :ENTER
   k/LEFT  :LEFT
   k/NINE  :NINE
   k/OPEN_SQUARE_BRACKET :LBRAK
   k/RIGHT :RIGHT
   k/SPACE :SPACE
   k/TAB   :TAB
   k/THREE :THREE
   k/UP    :UP
   k/ZERO  :ZERO})

(defn key-data [ev]
  (-> #{(key-name (.-keyCode ev))}
      (#(if (.-ctrlKey ev)  (conj % :CTRL)  %))
      (#(if (.-metaKey ev)  (conj % :CMD)   %))
      (#(if (.-shiftKey ev) (conj % :SHIFT) %))))
