(ns flense.ui.editor
  (:refer-clojure :exclude [chars rem])
  (:require [clojure.string :as str]
            [flense.parse :as p]
            [flense.util.dom :as udom :refer [rem]]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [xyzzy.core :as z])
  (:require-macros [cljs.core.async.macros :refer [go-loop]]))

(def ^:private MAX_CHARS_PER_LINE 72)

(defn class-name [classes]
  (->> classes (map name) (str/join " ")))

(defn presentational? [token-or-form]
  (contains? token-or-form :content))

(defn chars [token-or-form]
  (if (presentational? token-or-form)
    (count (:content token-or-form))
    (count (p/tree->str token-or-form))))

(defn line-count [text]
  (inc (int (/ (count text) (- MAX_CHARS_PER_LINE 2)))))

(defn delimiters [{:keys [selected? type]}]
  [[{:classes (cond-> #{:delimiter type :left} selected? (conj :selected))
     :content (case type :seq "(" :vec "[" :map "{" :set "#{")}]
   [{:classes (cond-> #{:delimiter type :right} selected? (conj :selected))
     :content (case type :seq ")" :vec "]" (:map :set) "}")}]])

(defn spacer
  ([] (spacer 1))
  ([n] [{:classes #{:spacer}
         :content (str/join (repeat n \space))}]))

(defn annotate-head [form]
  (if (and (= (:type form) :seq) (seq (:children form)))
    (update-in form [:children 0] assoc :head? true)
    form))

(defn ->tokens [form]
  (if (p/coll-node? form)
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

(defmethod ->lines* :default [form]
  (let [children (:children form)
        indent (if (#{:seq :set} (:type form)) (spacer 2) (spacer))
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
  (if (and (p/coll-node? form) (not (fits-on-own-line? form)))
    (->lines* (annotate-head form))
    [(->tokens form)]))

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
          #(om/update! form (p/parse-token (.. % -target -value)))
        :onKeyDown
          #(when-not ((:propagate-keypress? opts) % @form)
             (.stopPropagation %))
         :style #js {:width (rem (/ (chars form) 2))}
         :value (:text form)}))
    om/IDidMount
    (did-mount [_]
      (when (:selected? form)
        (udom/focus+select (om/get-node owner))))
    om/IDidUpdate
    (did-update [_ prev _]
      (let [input (om/get-node owner)]
        (if (:selected? form)
          (when (or (not (:selected? prev)) (p/placeholder-node? form))
            (udom/focus+select input))
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
          (if (p/placeholder-node? form)
            (udom/focus+select input)
            (udom/move-caret-to-end input)))))
    om/IDidUpdate
    (did-update [_ prev _]
      (let [input (om/get-node owner "content")]
        (if (:editing? form)
          (when-not (:editing? prev)
            (udom/move-caret-to-end input))
          (when (:editing? prev)
            (.blur input)))))))

(defn form-view [form owner opts]
  (reify om/IRender
    (render [_]
      (apply dom/div #js {:className "form"}
        (for [line (->lines form)]
          (apply dom/div #js {:className "line"}
            (for [token line]
              (cond
                (presentational? token)
                (dom/span #js {:className (class-name (:classes token))}
                  (:content token))
                (p/stringlike-node? token)
                (om/build stringlike-view token {:opts opts})
                :else
                (om/build atom-view token {:opts opts})))))))))

(defn editor-view [document owner opts]
  (reify
    om/IWillMount
    (will-mount [_]
      (go-loop []
        (let [action (<! (:edit-chan opts))]
          (when ((:pred action) @document)
            (om/transact! document [] (:edit action) (:tags action))))
        (recur)))
    om/IRender
    (render [_]
      (let [{:keys [tree]} (z/edit document assoc :selected? true)]
        (apply dom/div #js {:className "editor"}
          (om/build-all form-view (:children tree)
            {:opts (select-keys opts [:propagate-keypress?])}))))))
