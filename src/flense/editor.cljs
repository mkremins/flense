(ns flense.editor
  (:refer-clojure :exclude [chars rem])
  (:require [cljs.core.async :as async]
            [clojure.string :as str]
            [flense.model :as model]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [xyzzy.core :as z])
  (:require-macros [cljs.core.async.macros :refer [go-loop]]))

(def MAX_CHARS_PER_LINE 72)

(defn class-name [classes]
  (->> classes (map name) (str/join " ")))

(defn delimiter? [token]
  (contains? (:classes token) :delimiter))

(defn spacer? [token]
  (contains? (:classes token) :spacer))

(defn chars [token-or-form]
  (if (contains? token-or-form :content)
    (count (:content token-or-form))
    (count (model/tree->string token-or-form))))

(defn line-count [text]
  (inc (int (/ (count text) (- MAX_CHARS_PER_LINE 2)))))

(defn path-to [form]
  (filterv number? (om/path form)))

(defn delimiters [{:keys [selected? type] :as form}]
  [[{:classes (cond-> #{:delimiter type :left} selected? (conj :selected))
     :content (case type :seq "(" :vec "[" :map "{" :set "#{")
     :path (path-to form)}]
   [{:classes (cond-> #{:delimiter type :right} selected? (conj :selected))
     :content (case type :seq ")" :vec "]" (:map :set) "}")
     :path (path-to form)}]])

(defn spacer
  ([] (spacer 1))
  ([n] [{:classes #{:spacer}
         :content (str/join (repeat n \space))}]))

(defn annotate-head [form]
  (if (and (= (:type form) :seq) (seq (:children form)))
    (update-in form [:children 0] assoc :head? true)
    form))

(defn update-last [v f & args]
  (conj (pop v) (apply f (peek v) args)))

(defn ->tokens [form]
  (if (model/collection? form)
    (let [[opener closer] (delimiters form)]
      (concat opener
        (->> (:children (annotate-head form))
             (map ->tokens)
             (interpose (spacer))
             (apply concat))
        closer))
    [form]))

(defn fits-on-line? [line form]
  (<= (+ (apply + (map chars line)) 1 (chars form)) MAX_CHARS_PER_LINE))

(defn fits-on-own-line? [form]
  (<= (chars form) MAX_CHARS_PER_LINE))

(defn has-content? [line]
  (not-every? #(contains? (:classes %) :spacer) line))

(defmulti ->lines*
  (fn [form]
    (when (and (= (:type form) :seq)
               (= (:type (first (:children form))) :symbol))
      (symbol (get-in form [:children 0 :text])))))

(declare ->lines)

(defn pair->lines [[left right]]
  (if (> (+ (chars left) 1 (chars right)) MAX_CHARS_PER_LINE)
    (vec (concat (->lines left) (map #(concat (spacer 2) %) (->lines right))))
    [(concat (->tokens left) (spacer) (->tokens right))]))

(defn pairs->lines [forms]
  (let [pairs (partition 2 forms)
        extra (when (odd? (count forms)) (last forms))
        lines (cond-> (mapcat pair->lines pairs) extra (concat (->lines extra)))]
    (vec lines)))

(defn map->lines [form]
  (let [[opener closer] (delimiters form)
        lines (pairs->lines (:children form))
        init-line (concat opener (first lines))
        rest-lines (map #(concat (spacer) %) (rest lines))]
    (update-last `[~init-line ~@rest-lines] concat closer)))

(defn indent-size [form]
  (case (:type form)
    :seq (let [head (first (:children form))]
           (+ 2 (when (#{:symbol :keyword} (:type head)) (count (:text head)))))
    :set 2
    1))

(defmethod ->lines* :default [form]
  (let [children (:children form)
        indent (spacer (indent-size form))
        [opener closer] (delimiters form)]
    (loop [lines []
           line (concat opener (->tokens (first children)))
           children (rest children)]
      (if-let [child (first children)]
        (cond
          (fits-on-line? line child) ; append child to current line
          (let [line (cond-> line (has-content? line) (concat (spacer)))]
            (recur lines
                   (concat line (->tokens child))
                   (rest children)))
          (fits-on-own-line? child) ; insert a new line containing child
          (recur (conj lines line)
                 (concat indent (->tokens child))
                 (rest children))
          :else ; split child across multiple lines and insert them all
          (let [lines (cond-> lines (has-content? line) (conj line))]
            (recur (vec (concat lines (map #(concat indent %) (->lines child))))
                   (if (rest children) indent ())
                   (rest children))))
        (if (has-content? line)
          (conj lines (concat line closer))
          (conj (pop lines) (concat (peek lines) closer)))))))

(defn ->lines [form]
  (if (or (not (model/collection? form)) (fits-on-own-line? form))
    [(->tokens form)]
    (case (:type form)
      :map (map->lines form)
      :seq (->lines* (annotate-head form))
      (:vec :set) (->lines* form))))

;; DOM utils

(defn focus+select [input]
  (doto input .focus .select))

(defn move-caret-to-end [input]
  (let [idx (count (.-value input))]
    (set! (.-selectionStart input) idx)
    (set! (.-selectionEnd input) idx)))

(defn rem [n]
  (str n "rem"))

;; Om components

(defn atom-view [form owner opts]
  (reify
    om/IRender
    (render [_]
      (dom/input #js {
        :className
          (class-name
           (cond-> #{:atom (:type form)}
                   (:head? form) (conj :head)
                   (:selected? form) (conj :selected)
                   (:collapsed-form form) (conj :macroexpanded)))
        :onChange
          #(om/update! form (model/string->atom (.. % -target -value)))
        :onClick
          #(async/put! (:nav-chan opts) (path-to form))
        :onKeyDown
          #(when-not ((:propagate-keypress? opts) % @form)
             (.stopPropagation %))
         :style #js {:width (rem (/ (chars form) 2))}
         :value (:text form)}))
    om/IDidMount
    (did-mount [_]
      (when (:selected? form)
        (focus+select (om/get-node owner))))
    om/IDidUpdate
    (did-update [_ prev _]
      (let [input (om/get-node owner)]
        (if (:selected? form)
          (when (or (not (:selected? prev)) (model/placeholder? form))
            (focus+select input))
          (when (:selected? prev)
            (.blur input)))))))

(defn stringlike-view [form owner opts]
  (reify
    om/IRender
    (render [_]
      (dom/div #js {
        :className
          (class-name
           (cond-> #{:stringlike (:type form)}
                   (:editing? form) (conj :editing)
                   (:selected? form) (conj :selected)))}
        (let [text (str/replace (:text form) #"\s+" " ")]
          (dom/textarea #js {
            :onChange
              #(om/update! form :text (.. % -target -value))
            :onClick
              #(async/put! (:nav-chan opts) (path-to form))
            :onKeyDown
              #(when-not ((:propagate-keypress? opts) % @form)
                 (.stopPropagation %))
            :ref "content"
            :style #js {
              :height (rem (* 1.2 (line-count text)))
              :width  (rem (/ (min (count text) MAX_CHARS_PER_LINE) 2))}
            :value text}))))
    om/IDidMount
    (did-mount [_]
      (when (:editing? form)
        (let [input (om/get-node owner "content")]
          (if (model/placeholder? form)
            (focus+select input)
            (move-caret-to-end input)))))
    om/IDidUpdate
    (did-update [_ prev _]
      (let [input (om/get-node owner "content")]
        (if (:editing? form)
          (when-not (:editing? prev)
            (move-caret-to-end input))
          (when (:editing? prev)
            (.blur input)))))))

(defn delimiter-view [token owner opts]
  (reify om/IRender
    (render [_]
      (dom/span #js {
        :className (class-name (:classes token))
        :onClick #(async/put! (:nav-chan opts) (:path token))}
        (:content token)))))

(defn form-view [form owner opts]
  (reify om/IRender
    (render [_]
      (apply dom/div #js {:className "toplevel"}
        (for [line (->lines form)]
          (apply dom/div #js {:className "line"}
            (for [token line]
              (condp apply [token]
                spacer? (dom/span #js {:className "spacer"} (:content token))
                delimiter? (om/build delimiter-view token {:opts opts})
                model/stringlike? (om/build stringlike-view token {:opts opts})
                (om/build atom-view token {:opts opts})))))))))

(defn editor-view [document owner opts]
  (reify
    om/IInitState
    (init-state [_]
      {:nav-chan (async/chan)})
    om/IWillMount
    (will-mount [_]
      (go-loop []
        (when-let [action (<! (:edit-chan opts))]
          (when ((:pred action) @document)
            (om/transact! document [] (:edit action) (:tags action)))
          (recur)))
      (go-loop []
        (when-let [new-path (<! (om/get-state owner :nav-chan))]
          (om/transact! document []
            #(-> % (z/edit dissoc :editing?) (assoc :path new-path)))
          (recur))))
    om/IWillUnmount
    (will-unmount [_]
      (async/close! (om/get-state owner :nav-chan)))
    om/IRenderState
    (render-state [_ {:keys [nav-chan]}]
      (let [{:keys [tree]} (z/edit document assoc :selected? true)]
        (apply dom/div #js {:className "flense"}
          (om/build-all form-view (:children tree)
            {:opts (-> opts (select-keys [:propagate-keypress?])
                            (assoc :nav-chan nav-chan))}))))))
