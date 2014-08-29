(ns flense.ui.editor.layout
  (:require [flense.ui.editor :refer [delimiters ->lines ->lines* spacer ->tokens]]))

(defn- update-last [v f & args]
  (conj (pop v) (apply f (peek v) args)))

;; iflike

(defn iflike->lines [form]
  (let [[opener closer] (delimiters form)
        [head test then else & more] (:children form)
        init-line (concat opener (->tokens head) (spacer) (->tokens test))
        then-lines (map #(concat (spacer 2) %) (->lines then))
        else-lines (map #(concat (spacer 2) %) (->lines else))
        more-lines (map #(concat (spacer 2) %) (mapcat ->lines more))
        lines `[~init-line ~@then-lines ~@else-lines ~@more-lines]]
    (update-last lines concat closer)))

(doseq [iflike '[if if-let if-not if-some]]
  (defmethod ->lines* iflike [form] (iflike->lines form)))

;; letlike

(defn bpair->tokens [[bform bval]]
  (concat (->tokens bform) (spacer) (->tokens bval)))

(defn bvec->lines [form]
  (let [[opener closer] (delimiters form)
        children (:children form)
        pairs (partition 2 children)
        extra (when (odd? (count children)) (last children))]
    (if (seq pairs)
      (let [init-line (concat opener (bpair->tokens (first pairs)))
            rest-lines (mapv #(concat (spacer) (bpair->tokens %)) (rest pairs))
            rest-lines (cond-> rest-lines extra (conj (concat (spacer) (->tokens extra))))
            lines (vec (concat [init-line] rest-lines))]
        (update-last lines concat closer))
      [(concat opener (when extra (->tokens extra)) closer)])))

(defn letlike->lines [form]
  (let [[head bvec & body] (:children form)]
    (if (= (:type bvec) :vec)
      (let [[opener closer] (delimiters form)
            bvec-lines (bvec->lines bvec)
            body-lines (mapv #(concat (spacer 2) %) (mapcat ->lines body))
            body-lines (update-last body-lines concat closer)
            init-line (concat opener (->tokens head) (spacer) (first bvec-lines))
            bvec-indent (spacer (+ (count (:text head)) 2))
            bvec-lines (map #(concat bvec-indent %) (rest bvec-lines))]
        `[~init-line ~@bvec-lines ~@body-lines])
      ((get-method ->lines* :default) form))))

(doseq [letlike '[binding let loop]]
  (defmethod ->lines* letlike [form] (letlike->lines form)))

;; whenlike

(defn whenlike->lines [form]
  (let [[opener closer] (delimiters form)
        [head test & body] (:children form)
        init-line (concat opener (->tokens head) (spacer) (->tokens test))
        body-lines (mapv #(concat (spacer 2) %) (mapcat ->lines body))
        body-lines (update-last body-lines concat closer)]
    `[~init-line ~@body-lines]))

(doseq [whenlike '[when when-first when-let when-not when-some]]
  (defmethod ->lines* whenlike [form] (whenlike->lines form)))
