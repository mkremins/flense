(ns flense.edit
  (:require [flense.parse :as p]
            [flense.util :refer [delete insert lconj update]]
            [flense.zip :as z]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clipboard functionality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^{:dynamic true :private true} *clipboard* nil)

(defn copy-sexp! [loc]
  (set! *clipboard* (z/node loc))
  loc)

(defn paste-sexp [loc]
  (if (and *clipboard* (p/placeholder-node? (z/node loc)))
      (z/replace loc *clipboard*)
      loc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use these with `edit`
;;   ex: `(om/transact! app-state edit wrap-round)`
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- toggle-dispatch* [node]
  (update node :type
   #(or ({:map    :set
          :set    :map
          :seq    :fn
          :fn     :seq
          :string :regex
          :regex  :string} %) %)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use these with `edit-parent`
;;   ex: `(om/transact! app-state edit-parent slurp-child-right)`
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- delete-child* [parent i]
  (update parent :children delete i))

(defn- delete-leftmost* [parent]
  (update parent :children delete 0))

(defn- delete-rightmost* [parent]
  (update parent :children #(if (empty? %) % (pop %))))

(defn- insert-child* [parent i child]
  (update parent :children insert i child))

(defn- insert-leftmost* [parent _ child]
  (update parent :children lconj child))

(defn- insert-rightmost* [parent _ child]
  (update parent :children conj child))

(defn- barf-child-left* [parent i]
  (if-let [barfed (get-in parent [:children i :children 0])]
          (-> parent
              (insert-child* i barfed)
              (update-in [:children (inc i)] delete-leftmost*))
          parent))

(defn- barf-child-right* [parent i]
  (if-let [barfed (last (get-in parent [:children i :children]))]
          (-> parent
              (insert-child* (inc i) barfed)
              (update-in [:children i] delete-rightmost*))
          parent))

(defn- raise-child* [parent i]
  (get-in parent [:children i]))

(defn- slurp-child-left* [parent i]
  (let [slurped (get-in parent [:children (dec i)])]
    (if (and slurped (z/branch? (get-in parent [:children i])))
        (-> parent
            (update-in [:children i] insert-leftmost* nil slurped)
            (delete-child* (dec i)))
        parent)))

(defn- slurp-child-right* [parent i]
  (let [slurped (get-in parent [:children (inc i)])]
    (if (and slurped (z/branch? (get-in parent [:children i])))
        (-> parent
            (update-in [:children i] insert-rightmost* nil slurped)
            (delete-child* (inc i)))
        parent)))

(defn- splice-child* [parent i]
  (let [child (get-in parent [:children i])]
    (if (z/branch? child)
        (-> (reduce (fn [node grandchild]
                      (insert-child* node (inc i) grandchild))
                    parent (reverse (:children child)))
            (delete-child* i))
        parent)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; public API wrapping the above
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn barf-left [loc]
  (if (let [node (z/node loc)]
        (and (z/branch? node) (seq (:children node))))
      (let [new-loc (z/edit-parent loc barf-child-left*)]
        (if (and new-loc (not= new-loc loc))
            (z/child new-loc (-> loc :path peek inc))
            loc))
      loc))

(defn barf-right [loc]
  (if (let [node (z/node loc)]
        (and (z/branch? node) (seq (:children node))))
      (-> loc
          (z/edit-parent barf-child-right*)
          (z/child (-> loc :path peek)))
      loc))

(defn delete-sexp [loc]
  (if (p/placeholder-node? (z/node loc))
      (let [new-loc (z/edit-parent loc delete-child*)]
        (if (empty? (:children (z/node new-loc)))
            new-loc
            (z/child new-loc (max (-> loc :path peek dec) 0))))
      (z/replace loc p/placeholder)))

(defn insert-right [loc node]
  (if-let [right-loc (z/right loc)]
          (-> right-loc
              (z/edit-parent insert-child* node)
              (z/child (-> loc :path peek inc)))
          (-> loc (z/edit-parent insert-rightmost* node) z/down z/rightmost)))

(defn slurp-left [loc]
  (if (-> loc z/node z/branch?)
      (let [new-loc (z/edit-parent loc slurp-child-left*)]
        (if (and new-loc (not= new-loc loc))
            (z/child new-loc (max (-> loc :path peek dec) 0))
            loc))
      loc))

(defn slurp-right [loc]
  (if (-> loc z/node z/branch?)
      (-> loc
          (z/edit-parent slurp-child-right*)
          (z/child (-> loc :path peek)))
      loc))

(defn toggle-dispatch [loc]
  (z/edit loc toggle-dispatch*))

(defn wrap-sexp [type wrapped]
  {:type type :children [wrapped]})
