(ns flense.macros)

(defmacro extend-types [types protocol & fn-impls]
  (cons 'do
        (for [t types]
          `(extend-type ~t ~protocol ~@fn-impls))))
