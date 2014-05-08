(ns flense.keyboard
  (:require [goog.events.KeyCodes :as k]))

(def key-name
  {; letters ------------------------------------------------------------------
   k/A :A, k/B :B, k/C :C, k/D :D, k/E :E, k/F :F, k/G :G, k/H :H, k/I :I,
   k/J :J, k/K :K, k/L :L, k/M :M, k/N :N, k/O :O, k/P :P, k/Q :Q, k/R :R,
   k/S :S, k/T :T, k/U :U, k/V :V, k/W :W, k/X :X, k/Y :Y, k/Z :Z,

   ; digits -------------------------------------------------------------------
   k/ONE :ONE, k/TWO :TWO, k/THREE :THREE, k/FOUR :FOUR, k/FIVE :FIVE,
   k/SIX :SIX, k/SEVEN :SEVEN, k/EIGHT :EIGHT, k/NINE :NINE, k/ZERO :ZERO,

   ; arrows -------------------------------------------------------------------
   k/DOWN :DOWN, k/LEFT :LEFT, k/RIGHT :RIGHT, k/UP :UP,

   ; punctuation --------------------------------------------------------------
   k/BACKSLASH             :BKSLASH
   k/CLOSE_SQUARE_BRACKET  :RBRAK
   k/COMMA                 :COMMA
   k/DASH                  :DASH
   k/EQUALS                :EQUALS
   k/OPEN_SQUARE_BRACKET   :LBRAK
   k/PERIOD                :DOT
   k/SEMICOLON             :SEMI
   k/SINGLE_QUOTE          :QUOTE
   k/SLASH                 :SLASH

   ; other --------------------------------------------------------------------
   k/BACKSPACE             :BKSPACE
   k/CAPS_LOCK             :CAPS
   k/ENTER                 :ENTER
   k/ESC                   :ESC
   k/SPACE                 :SPACE
   k/TAB                   :TAB})

(defn key-data [ev]
  (-> #{(key-name (.-keyCode ev))}
      (#(if (.-altKey ev)   (conj % :ALT)   %))
      (#(if (.-ctrlKey ev)  (conj % :CTRL)  %))
      (#(if (.-metaKey ev)  (conj % :CMD)   %))
      (#(if (.-shiftKey ev) (conj % :SHIFT) %))))
