(ns flense.layout
  (:refer-clojure :exclude [chars])
  (:require [flense.model :as model]
            [om.core :as om]))

;; render forms to tokens
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def MAX_CHARS_PER_LINE 72)

(defn delimiter? [token]
  (contains? (:classes token) :delimiter))

(defn spacer? [token]
  (contains? (:classes token) :spacer))

(defn chars [token-or-form]
  (if (contains? token-or-form :content)
    (count (:content token-or-form))
    (count (model/tree->string token-or-form))))

(defn text-height [text]
  (inc (int (/ (count text) (- MAX_CHARS_PER_LINE 2)))))

(defn text-width [text]
  (min (count text) MAX_CHARS_PER_LINE))

(defn path-to [form]
  (filterv number? (om/path form)))

(defn delimiters [{:keys [editing? selected? type] :as form}]
  (let [classes (cond-> #{:delimiter type}
                  editing? (conj :editing) selected? (conj :selected))]
    [[{:classes (conj classes :left)
       :content (model/left-delimiter type)
       :path (path-to form)}]
     [{:classes (conj classes :right)
       :content (model/right-delimiter type)
       :path (path-to form)}]]))

(defn spacer
  ([] (spacer 1))
  ([n] [{:classes #{:spacer}
         :content (apply str (repeat n \space))}]))

(defn annotate-head [form]
  (if (and (#{:fn :seq} (:type form)) (seq (:children form)))
    (update-in form [:children 0] assoc :head? true)
    form))

(defn ->tokens [form]
  (cond
    (model/collection? form)
    (let [[opener closer] (delimiters form)]
      (concat opener
              (->> (:children (annotate-head form))
                   (map ->tokens) (interpose (spacer)) (apply concat))
              closer))
    (model/stringlike? form)
    (let [[opener closer] (delimiters form)]
      (concat opener [form] closer))
    :else
    [form]))

;; lay out tokens across multiple lines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn update-last [v f & args]
  (conj (pop v) (apply f (peek v) args)))

(defn fits-on-line? [line form]
  (<= (+ (apply + (map chars line)) 1 (chars form)) MAX_CHARS_PER_LINE))

(defn fits-on-own-line? [form]
  (<= (chars form) MAX_CHARS_PER_LINE))

(defn has-content? [line]
  (not-every? #(contains? (:classes %) :spacer) line))

(defmulti ->lines*
  (fn [form]
    (when (and (#{:fn :seq} (:type form))
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

(defn indent-size* [form]
  (if (= (:type form) :fn) 3 2))

(defn indent-size [form]
  (letfn [(head-size [form]
            (let [head (first (:children form))]
              (when (#{:symbol :keyword} (:type head)) (count (:text head)))))]
    (case (:type form)
      (:fn :seq) (+ (indent-size* form) (head-size form)), :set 2, 1)))

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
      (:fn :seq) (->lines* (annotate-head form))
      (:vec :set) (->lines* form))))

;; lay out clojure.core macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; simple "header+body" layout is good enough for most core macros

(defn header+body->lines [form headc]
  (let [[opener closer] (delimiters form)
        [inits rests] (split-at headc (:children form))
        init-line (concat opener (apply concat (interpose (spacer) (map ->tokens inits))))
        rest-lines (map #(concat (spacer (indent-size* form)) %) (mapcat ->lines rests))]
    (update-last `[~init-line ~@rest-lines] concat closer)))

(def header-counts
  '{-> 2, ->> 2, as-> 3, def 2, definline 2, definterface 2, defmacro 2, defmethod 3, defmulti 2,
    defn 2, defn- 2, defonce 2, defprotocol 2, defrecord 3, defstruct 2, deftype 3, do 2,
    dotimes 2, extend 2, extend-protocol 2, extend-type 2, if 2, if-let 2, if-not 2, if-some 2,
    proxy 3, reify 1, some-> 2, some->> 2, specify 2, specify! 2, when 2, when-first 2, when-let 2,
    when-not 2, when-some 2})

(doseq [[core-macro headc] header-counts]
  (defmethod ->lines* core-macro [form] (header+body->lines form headc)))

;; "header+pairs" works for several other core macros

(defn header+pairs->lines [form headc]
  (let [[opener closer] (delimiters form)
        [inits rests] (split-at headc (:children form))
        init-line (concat opener (apply concat (interpose (spacer) (map ->tokens inits))))
        rest-lines (map #(concat (spacer (indent-size* form)) %) (pairs->lines rests))]
    (update-last `[~init-line ~@rest-lines] concat closer)))

(def paired-header-counts
  '{case 2, cond 1, cond-> 2, cond->> 2, condp 3})

(doseq [[core-macro headc] paired-header-counts]
  (defmethod ->lines* core-macro [form] (header+pairs->lines form headc)))

;; letlike core macros need their own layout algorithm

(defn letlike->lines [form]
  (let [[head bvec & body] (:children form)]
    (if (= (:type bvec) :vec)
      (let [[opener closer] (delimiters form)
            indent (indent-size* form)
            bvec-lines (map->lines bvec)
            body-lines (mapv #(concat (spacer indent) %) (mapcat ->lines body))
            body-lines (update-last body-lines concat closer)
            init-line (concat opener (->tokens head) (spacer) (first bvec-lines))
            bvec-indent (spacer (+ (count (:text head)) indent))
            bvec-lines (map #(concat bvec-indent %) (rest bvec-lines))]
        `[~init-line ~@bvec-lines ~@body-lines])
      ((get-method ->lines* :default) form))))

(doseq [letlike '[binding doseq for let loop]]
  (defmethod ->lines* letlike [form] (letlike->lines form)))
