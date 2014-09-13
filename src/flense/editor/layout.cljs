(ns flense.editor.layout
  (:require [flense.editor
             :refer [chars delimiters ->lines ->lines* map->lines MAX_CHARS_PER_LINE pairs->lines
                     spacer ->tokens update-last]]))

(defn indent-size [form]
  (if (= (:type form) :fn) 3 2))

;; simple "header+body" layout is good enough for most core macros

(defn header+body->lines [form headc]
  (let [[opener closer] (delimiters form)
        [inits rests] (split-at headc (:children form))
        init-line (concat opener (apply concat (interpose (spacer) (map ->tokens inits))))
        rest-lines (map #(concat (spacer (indent-size form)) %) (mapcat ->lines rests))]
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
        rest-lines (map #(concat (spacer (indent-size form)) %) (pairs->lines rests))]
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
            indent (indent-size form)
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
