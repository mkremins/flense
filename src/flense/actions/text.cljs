(ns flense.actions.text
  (:require [flense.model :as model]
            [xyzzy.core :as z]))

(defn insert-char [c loc]
  (let [node (z/node loc)]
    (when-let [text (:text node)]
      (if (model/placeholder? node)
        (z/replace loc (model/string->atom c))
        (z/replace loc (model/string->atom (str text c)))))))

(def actions
  (let [uppers (map (comp js/String.fromCharCode (partial + 65)) (range 26))
        lowers (map (comp js/String.fromCharCode (partial + 97)) (range 26))
        digits (map str (range 10))
        puncts [\. \! \? \$ \% \&  \+ \- \* \/ \= \< \> \_ \: \' \\ \|]]
    (into {} (for [c (concat uppers lowers digits puncts)]
               [(keyword "text" (str "insert-" c)) (partial insert-char c)]))))
