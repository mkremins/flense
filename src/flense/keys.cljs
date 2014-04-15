(ns flense.keys)

(defn keycode
  "Given a key event `ev`, returns the integer keycode of the associated key."
  [ev]
  (or (.-key ev) (.-keyCode ev) (.-which ev)))

(def key-name
  "Given an integer keycode, returns a human-readable keyword naming the
   corresponding key."
  { 18 :ALT
    17 :CTRL
     8 :DEL
    40 :DOWN
    13 :ENTER
    27 :ESC
   219 :LBRAK
    37 :LEFT
    49 :NUM_1
    50 :NUM_2
    51 :NUM_3
    52 :NUM_4
    53 :NUM_5
    54 :NUM_6
    55 :NUM_7
    56 :NUM_8
    57 :NUM_9
    48 :NUM_0
   221 :RBRAK
    39 :RIGHT
    16 :SHIFT
    32 :SPACE
     9 :TAB
    38 :UP})

(def ev->key
  "Given a key event, returns a human-readable keyword naming the associated
   key."
  (comp key-name keycode))
