(ns flense.ui.editor
  (:refer-clojure :exclude [chars rem])
  (:require [clojure.string :as str]
            [flense.keymap :as keymap]
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

(defn delimiter [{:keys [selected? type]} l-or-r]
  {:classes (cond-> #{:delimiter type l-or-r} selected? (conj :selected))
   :content (case [type l-or-r]
              [:seq :left] "(",  [:seq :right] ")"
              [:vec :left] "[",  [:vec :right] "]"
              [:map :left] "{",  [:map :right] "}"
              [:set :left] "#{", [:set :right] "}")})

(def spacer
  {:classes #{:spacer} :content \space})

(defn annotate-head [form]
  (if (and (= (:type form) :seq) (seq (:children form)))
    (update-in form [:children 0] assoc :head? true)
    form))

(defn ->tokens [form]
  (if (p/coll-node? form)
    (concat
      [(delimiter form :left)]
      (->> (:children (annotate-head form))
           (map ->tokens)
           (interpose [spacer])
           (apply concat))
      [(delimiter form :right)])
    [form]))

(defn fits-on-line? [line form]
  (<= (+ (apply + (map chars line)) 1 (chars form)) MAX_CHARS_PER_LINE))

(defn fits-on-own-line? [form]
  (<= (chars form) MAX_CHARS_PER_LINE))

(defn has-content? [line]
  (not-every? #(contains? (:classes %) :spacer) line))

(defn ->lines [form]
  (if (and (p/coll-node? form) (not (fits-on-own-line? form)))
    (let [children (:children (annotate-head form))
          indent (if (= (:type form) :seq) [spacer spacer] [spacer])]
      (loop [lines []
             line (concat [(delimiter form :left)] (->tokens (first children)))
             children (rest children)]
        (if-let [child (first children)]
          (cond
            (fits-on-line? line child) ; append child to current line
            (let [line (cond-> line (has-content? line) (concat [spacer]))]
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
            (conj lines (concat line [(delimiter form :right)]))
            (conj (pop lines) (concat (peek lines) [(delimiter form :right)]))))))
    [(->tokens form)]))

(defn line-count [text]
  (inc (int (/ (count text) (- MAX_CHARS_PER_LINE 2)))))

;; Om components

(defn atom-view [form owner]
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
        :onKeyDown ; prevent delete keybind unless text fully selected
          #(when (and (= (:name (keymap/bound-action %)) :paredit/remove)
                      (not (udom/fully-selected? (.-target %))))
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

(defn stringlike-view [form owner]
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
            :onKeyDown ; prevent keybinds (except those that end editing)
              #(when-not (-> % keymap/bound-action :name
                             #{:flense/text-command :move/up :paredit/insert-outside})
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

(defn form-view [form owner]
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
                (om/build stringlike-view token)
                :else
                (om/build atom-view token)))))))))

(defn editor-view [document owner]
  (reify
    om/IWillMount
    (will-mount [_]
      (let [edit-chan (om/get-shared owner :edit-chan)]
        (go-loop []
          (let [{:keys [cases]} (<! edit-chan)]
            (when-let [case (first (filter #((:pred %) @document) cases))]
              (om/transact! document [] (:edit case) (:tags case))))
          (recur))))
    om/IRender
    (render [_]
      (let [{:keys [tree]} (z/edit document assoc :selected? true)]
        (apply dom/div #js {:className "editor"}
          (om/build-all form-view (:children tree)))))))
