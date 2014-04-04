(ns flense.keys)

(defn keycode
  "Given a key event `ev`, returns the integer keycode of the associated key."
  [ev]
  (or (.-key ev) (.-keyCode ev) (.-which ev)))

(def key-name
  "Given an integer keycode, returns a human-readable keyword naming the
   corresponding key."
  {17 :CTRL 
    8 :DEL
   40 :DOWN
   13 :ENTER
   27 :ESC
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
   39 :RIGHT
   32 :SPACE
    9 :TAB
   38 :UP})

(def ev->key
  "Given a key event, returns a human-readable keyword naming the associated
   key."
  (comp key-name keycode))

(def ^:private held-keys (atom #{}))

(defn held?
  "Returns truthy if you have registered `key` as modal with `trap-modal-keys!`
   and `key` is currently held, else falsey."
  [key]
  (@held-keys key))

(defn trap-modal-keys!
  "Begins listening for keyup and keydown events on each key in `keys`. Use in
   conjunction with the `held?` function to test if a key is currently held."
  [keys]
  (let [modal? (set keys)]
    (.addEventListener js/window "keydown"
                       (fn [ev]
                         (let [key (ev->key ev)]
                           (when (modal? key)
                             (swap! held-keys conj key)))))
    (.addEventListener js/window "keyup"
                       (fn [ev]
                         (let [key (ev->key ev)]
                           (when (modal? key)
                             (swap! held-keys disj key)))))))
