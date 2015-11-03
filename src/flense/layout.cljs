(ns flense.layout
  (:refer-clojure :exclude [chars])
  (:require [flense.model :as model]))

;;; render forms to tokens

(def ^:dynamic *line-length*)

(defn delimiter? [token]
  (contains? (:classes token) :delimiter))

(defn spacer? [token]
  (contains? (:classes token) :spacer))

(defn chars [token]
  (if (model/stringlike? token)
    (+ 2 (min (count (:text token)) (- *line-length* 8)))
    (count (or (:text token) (model/tree->string token)))))

(defn delimiter [opener? {:keys [editing? selected? type] :as form}]
  [{:classes (cond-> #{:delimiter (if opener? :left :right) type}
                     editing? (conj :editing) selected? (conj :selected))
    :path (:path form)
    :text ((if opener? model/opener model/closer) type)}])

(def opener (partial delimiter true))
(def closer (partial delimiter false))

(defn spacer
  ([] (spacer 1))
  ([n] [{:classes #{:spacer}
         :text (apply str (repeat n \space))}]))

(defn annotate-head [form]
  (if (and (#{:fn :seq} (:type form)) (seq (:children form)))
    (update-in form [:children 0] assoc :head? true)
    form))

(defn ->tokens [form]
  (condp #(%1 %2) form
    model/coll?
      (concat (opener form)
              (->> (:children (annotate-head form))
                   (map ->tokens) (interpose (spacer)) (apply concat))
              (closer form))
    model/stringlike?
      (concat (opener form)
              [(assoc form :max-width (- *line-length* 8))]
              (closer form))
    ;else
      [form]))

;;; lay out tokens across multiple lines

(defn head-symbol [form]
  (when (and (#{:fn :seq} (:type form))
               (= (:type (first (:children form))) :symbol))
    (symbol (get-in form [:children 0 :text]))))

(def always-multiline
  '#{binding definline definterface defmacro defmethod defn defn- defonce
     defprotocol defrecord defstruct deftype do doseq dotimes extend
     extend-protocol extend-type for let loop proxy reify specify specify!})

(defn always-multiline? [form]
  (or (contains? always-multiline (head-symbol form))
      (some always-multiline? (:children form))))

(defn close-off [lines closer]
  (conj (pop lines) (concat (peek lines) closer)))

(defn fits-on-line? [line form]
  (and (<= (apply + (chars form) 1 (map chars line)) *line-length*)
       (not (always-multiline? form))))

(defn fits-on-own-line? [form]
  (and (<= (chars form) *line-length*)
       (not (always-multiline? form))))

(defn has-content? [line]
  (not-every? #(contains? (:classes %) :spacer) line))

(defmulti ->lines* head-symbol)

(declare ->lines)

(defn pair->lines [[left right]]
  (if (> (+ (chars left) 1 (chars right)) *line-length*)
    (vec (concat (->lines left) (map #(concat (spacer 2) %) (->lines right))))
    [(concat (->tokens left) (spacer) (->tokens right))]))

(defn pairs->lines [forms]
  (let [pairs (partition 2 forms)
        extra (when (odd? (count forms)) (last forms))
        lines (cond-> (mapcat pair->lines pairs) extra (concat (->lines extra)))]
    (vec lines)))

(defn map->lines [form]
  (let [lines (pairs->lines (:children form))
        init-line (concat (opener form) (first lines))
        rest-lines (map #(concat (spacer) %) (rest lines))]
    (close-off `[~init-line ~@rest-lines] (closer form))))

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
        indent (spacer (indent-size form))]
    (loop [lines []
           line (concat (opener form) (->tokens (first children)))
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
          (conj lines (concat line (closer form)))
          (close-off lines (closer form)))))))

(defn ->lines
  ([form] (->lines form *line-length*))
  ([form line-length]
    (binding [*line-length* line-length]
      (if (or (not (model/coll? form)) (fits-on-own-line? form))
        [(->tokens form)]
        (case (:type form)
          :map (map->lines form)
          (:fn :seq) (->lines* (annotate-head form))
          (:vec :set) (->lines* form))))))

;;; lay out clojure.core macros

;; simple "header+body" layout is good enough for most core macros

(defn header+body->lines [form headc]
  (let [[inits rests] (split-at (inc headc) (:children form))
        init-line (apply concat (opener form) (interpose (spacer) (map ->tokens inits)))
        rest-lines (map #(concat (spacer (indent-size* form)) %) (mapcat ->lines rests))]
    (close-off `[~init-line ~@rest-lines] (closer form))))

(def header-counts
  '{-> 1, ->> 1, as-> 2, def 1, definline 1, definterface 1, defmacro 1, defmethod 2,
    defmulti 1, defn 1, defn- 1, defonce 1, defprotocol 1, defrecord 2, defstruct 1,
    deftype 2, do 1, dotimes 1, extend 1, extend-protocol 1, extend-type 1, fn 1, if 1,
    if-let 1, if-not 1, if-some 1, proxy 2, reify 0, some-> 1, some->> 1, specify 1,
    specify! 1, when 1, when-first 1, when-let 1, when-not 1, when-some 1})

(doseq [[core-macro headc] header-counts]
  (defmethod ->lines* core-macro [form] (header+body->lines form headc)))

;; "header+pairs" works for several other core macros

(defn header+pairs->lines [form headc]
  (let [[inits rests] (split-at (inc headc) (:children form))
        init-line (apply concat (opener form) (interpose (spacer) (map ->tokens inits)))
        rest-lines (map #(concat (spacer (indent-size* form)) %) (pairs->lines rests))]
    (close-off `[~init-line ~@rest-lines] (closer form))))

(def paired-header-counts
  '{case 1, cond 0, cond-> 1, cond->> 1, condp 2})

(doseq [[core-macro headc] paired-header-counts]
  (defmethod ->lines* core-macro [form] (header+pairs->lines form headc)))

;; letlike core macros need their own layout algorithm

(defn letlike->lines [form]
  (let [[head bvec & body] (:children form)]
    (if (= (:type bvec) :vec)
      (let [indent (indent-size* form)
            [init & more] (map->lines bvec)
            init-line (concat (opener form) (->tokens head) (spacer) init)
            bvec-indent (spacer (+ (count (:text head)) indent))
            bvec-lines (map #(concat bvec-indent %) more)
            body-lines (map #(concat (spacer indent) %) (mapcat ->lines body))]
        (close-off `[~init-line ~@bvec-lines ~@body-lines] (closer form)))
      ((get-method ->lines* :default) form))))

(doseq [letlike '[binding doseq for let loop]]
  (defmethod ->lines* letlike [form] (letlike->lines form)))
