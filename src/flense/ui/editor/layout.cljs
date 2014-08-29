(ns flense.ui.editor.layout
  (:require [flense.ui.editor :refer [delimiters ->lines ->lines* spacer ->tokens]]))

(defn whenlike->lines [form]
  (let [[opener closer] (delimiters form)
        [head test & body] (:children form)
        init-line (concat opener (->tokens head) (spacer) (->tokens test))
        body-lines (mapv #(concat (spacer 2) %) (mapcat ->lines body))
        body-lines (conj (pop body-lines) (concat (peek body-lines) closer))]
    `[~init-line ~@body-lines]))

(doseq [whenlike '[when when-first when-let when-not when-some]]
  (defmethod ->lines* whenlike [form] (whenlike->lines form)))
