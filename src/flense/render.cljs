(ns flense.render
  (:require-macros [flense.macros :refer [extend-types]]))

(defprotocol IRender
  (-render [this path cursor]))

(defn- render-coll [type items path cursor]
  (let [classes (str (name type) (when (= path cursor) " selected"))]
    [:div.coll {:class classes}
               (map-indexed (fn [idx item]
                              (-render item (conj path idx) cursor))
                            items)]))

(defn- render-token [text path cursor]
  [:span.token (when (= path cursor) {:class "selected"}) text])

(extend-protocol IRender
  List
  (-render [this path cursor]
    (render-coll :seq this path cursor))

  PersistentHashMap
  (-render [this path cursor]
    (render-coll :map (interleave (keys this) (vals this)) path cursor))

  PersistentHashSet
  (-render [this path cursor]
    (render-coll :set this path cursor))

  PersistentVector
  (-render [this path cursor]
    (render-coll :vec this path cursor))

  js/String
  (-render [this path cursor]
    (render-token (str "\"" this "\"") path cursor)))

(extend-types [Keyword js/Number Symbol]
  IRender
  (-render [this path cursor]
    (render-token (str this) path cursor)))

(defn root [state]
  (let [{:keys [tree cursor]} @state]
    [:div.flense (map-indexed (fn [idx item]
                                (-render item [idx] cursor))
                              tree)]))
