(ns flense.ranges)

(defn select-range! [text-range]
  (doto (.getSelection js/window)
    (.removeAllRanges)
    (.addRange text-range)))

(defn select-contents! [elem]
  (select-range! (doto (.createRange js/document)
                   (.selectNodeContents elem))))

(defn selected-range []
  (-> js/window (.getSelection) (.getRangeAt 0)))
