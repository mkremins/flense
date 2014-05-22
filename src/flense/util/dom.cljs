(ns flense.util.dom
  (:refer-clojure :exclude [rem]))

(defn focus+select [input]
  (doto input .focus .select))

(defn fully-selected? [input]
  (and (= (.-selectionStart input) 0)
       (= (.-selectionEnd input) (count (.-value input)))))

(defn in-view? [el]
  (let [rect (.getBoundingClientRect el)]
    (and (>= (.-top rect) 0)
         (<= (.-bottom rect) (.-innerHeight js/window)))))

(defn move-caret-to-end [input]
  (let [idx (count (.-value input))]
    (set! (.-selectionStart input) idx)
    (set! (.-selectionEnd input) idx)))

(defn px [n]
  (str n "px"))

(defn rem [n]
  (str n "rem"))

(defn scroll-into-view [el]
  (.scrollTo js/window 0 (+ (.-offsetTop el)
                            (- (.-innerHeight js/window))
                            (.-offsetHeight el)
                            60)))
