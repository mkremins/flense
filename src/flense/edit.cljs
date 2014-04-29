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
  (when-let [barfed (get-in parent [:children i :children 0])]
    (-> parent
        (insert-child* i barfed)
        (update-in [:children (inc i)] delete-leftmost*))))

(defn- barf-child-right* [parent i]
  (when-let [barfed (peek (get-in parent [:children i :children]))]
    (-> parent
        (insert-child* (inc i) barfed)
        (update-in [:children i] delete-rightmost*))))

(defn- join-child-left* [parent i]
  (let [joined (get-in parent [:children (dec i)])]
    (when (every? z/branch? [(get-in parent [:children i]) joined])
      (-> parent
          (update-in [:children i :children]
                     #(vec (concat (:children joined) %)))
          (update :children delete (dec i))))))

(defn- join-child-right* [parent i]
  (let [joined (get-in parent [:children (inc i)])]
    (when (every? z/branch? [(get-in parent [:children i]) joined])
      (-> parent
          (update-in [:children i :children]
                     #(vec (concat % (:children joined))))
          (update :children delete (inc i))))))

(defn- raise-child* [parent i]
  (get-in parent [:children i]))

(defn- slurp-child-left* [parent i]
  (let [slurped (get-in parent [:children (dec i)])]
    (when (and (z/branch? (get-in parent [:children i])) slurped)
      (-> parent
          (update-in [:children i] insert-leftmost* nil slurped)
          (delete-child* (dec i))))))

(defn- slurp-child-right* [parent i]
  (let [slurped (get-in parent [:children (inc i)])]
    (when (and (z/branch? (get-in parent [:children i])) slurped)
      (-> parent
          (update-in [:children i] insert-rightmost* nil slurped)
          (delete-child* (inc i))))))

(defn- splice-child* [parent i]
  (when (z/branch? (get-in parent [:children i]))
    (-> parent
        (update-in [:children i] :children)
        (update :children (comp vec flatten)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; public API wrapping the above
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn barf-left [loc]
  (or (-> loc
          (z/edit-parent barf-child-left*)
          (z/child (-> loc :path peek inc)))
      loc))

(defn barf-right [loc]
  (or (-> loc
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

(defn expand-sexp [loc]
  (z/edit loc p/expand-sexp))

(defn- find-placeholder [direction loc]
  (let [p-locs (z/find loc (comp p/placeholder-node? z/node) direction)
        p-loc  (first p-locs)
        p-loc  (if (= p-loc loc) (second p-locs) p-loc)]
    (or p-loc loc)))

(def find-placeholder-left  (partial find-placeholder z/backward))
(def find-placeholder-right (partial find-placeholder z/forward))

(defn insert-left [loc node]
  (-> loc
      (z/edit-parent insert-child* node)
      (z/child (-> loc :path peek))))

(defn insert-right [loc node]
  (if-let [right-loc (z/right loc)]
          (-> right-loc
              (z/edit-parent insert-child* node)
              (z/child (-> loc :path peek inc)))
          (-> loc (z/edit-parent insert-rightmost* node) z/down z/rightmost)))

(defn join-left [loc]
  (or (-> loc
          (z/edit-parent join-child-left*)
          (z/child (-> loc :path peek dec)))
      loc))

(defn join-right [loc]
  (or (-> loc
          (z/edit-parent join-child-right*)
          (z/child (-> loc :path peek)))
      loc))

(defn raise-sexp [loc]
  (z/edit-parent loc raise-child*))

(defn slurp-left [loc]
  (or (-> loc
          (z/edit-parent slurp-child-left*)
          (z/child (max (-> loc :path peek dec) 0)))
      loc))

(defn slurp-right [loc]
  (or (-> loc
          (z/edit-parent slurp-child-right*)
          (z/child (-> loc :path peek)))
      loc))

(defn splice-sexp [loc]
  (or (-> loc
          (z/edit-parent splice-child*)
          (z/child (-> loc :path peek)))
      loc))

(defn toggle-dispatch [loc]
  (z/edit loc toggle-dispatch*))

(defn wrap-sexp [type wrapped]
  {:type type :children [wrapped]})

(defn wrap-string [wrapped]
  {:type :string
   :children [(assoc wrapped
                :type :string-content
                :text (str (:form wrapped)))]})
