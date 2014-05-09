(ns flense.edit
  (:require [flense.parse :as p]
            [flense.util :refer [delete exchange insert update]]
            [flense.zip :as z]
            [flense.zip.util :as zu]))

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

(defn- make-sexp-type* [sexp type]
  (when (and (p/coll-node? sexp) (not (p/stringish-node? sexp)))
    (assoc sexp :type type)))

(defn- toggle-dispatch* [node]
  (update node :type
   #(or ({:map    :set
          :set    :map
          :seq    :fn
          :fn     :seq
          :string :regex
          :regex  :string} %) %)))

(defn- wrap-quote* [sexp]
  (when-not (p/coll-node? sexp)
    {:type :string
     :children [(assoc sexp
                  :type :string-content
                  :text (str (:form sexp)))]}))

(defn- wrap-sexp-type* [sexp type]
  {:type type :children [sexp]})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use these with `edit-parent`
;;   ex: `(om/transact! app-state edit-parent slurp-child-right)`
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- barf-child-left* [parent i]
  (let [barfer (get-in parent [:children i])]
    (when-not (p/stringish-node? barfer)
      (when-let [barfed (get-in barfer [:children 0])]
        (-> parent
            (zu/insert-child i barfed)
            (update-in [:children (inc i)] zu/delete-leftmost))))))

(defn- barf-child-right* [parent i]
  (let [barfer (get-in parent [:children i])]
    (when-not (p/stringish-node? barfer)
      (when-let [barfed (peek (:children barfer))]
        (-> parent
            (zu/insert-child (inc i) barfed)
            (update-in [:children i] zu/delete-rightmost))))))

(defn- join-child-left* [parent i]
  (let [joiner (get-in parent [:children i])
        joined (get-in parent [:children (dec i)])]
    (when (and (z/branch? joiner) (not (p/stringish-node? joiner))
               (z/branch? joined) (not (p/stringish-node? joined)))
      (-> parent
          (update-in [:children i :children]
                     #(vec (concat (:children joined) %)))
          (update :children delete (dec i))))))

(defn- join-child-right* [parent i]
  (let [joiner (get-in parent [:children i])
        joined (get-in parent [:children (inc i)])]
    (when (and (z/branch? joiner) (not (p/stringish-node? joiner))
               (z/branch? joined) (not (p/stringish-node? joined)))
      (-> parent
          (update-in [:children i :children]
                     #(vec (concat % (:children joined))))
          (update :children delete (inc i))))))

(defn- raise-child* [parent i]
  (when-not (p/stringish-node? parent)
    (get-in parent [:children i])))

(defn- slurp-child-left* [parent i]
  (let [slurper (get-in parent [:children i])
        slurped (get-in parent [:children (dec i)])]
    (when (and (z/branch? slurper) (not (p/stringish-node? slurper)) slurped)
      (-> parent
          (update-in [:children i] zu/insert-leftmost nil slurped)
          (zu/delete-child (dec i))))))

(defn- slurp-child-right* [parent i]
  (let [slurper (get-in parent [:children i])
        slurped (get-in parent [:children (inc i)])]
    (when (and (z/branch? slurper) (not (p/stringish-node? slurper)) slurped)
      (-> parent
          (update-in [:children i] zu/insert-rightmost nil slurped)
          (zu/delete-child (inc i))))))

(defn- splice-child* [parent i]
  (let [spliced (get-in parent [:children i])]
    (when (and (z/branch? spliced) (not (p/stringish-node? spliced)))
      (-> parent
          (update-in [:children i] :children)
          (update :children (comp vec flatten))))))

(defn- split-child-left* [split-idx parent i]
  (let [split          (get-in parent [:children i])
        [lefts rights] (map vec (split-at split-idx (:children split)))
        left-node      (assoc split :children lefts)
        right-node     (assoc split :children rights)]
    (-> parent
        (update :children insert i right-node)
        (update :children insert i left-node)
        (update :children delete (+ i 2)))))

(defn- split-child-right* [split-idx parent i]
  (let [split          (get-in parent [:children i])
        [lefts rights] (map vec (split-at (inc split-idx) (:children split)))
        left-node      (assoc split :children lefts)
        right-node     (assoc split :children rights)]
    (-> parent
        (update :children insert i right-node)
        (update :children insert i left-node)
        (update :children delete (+ i 2)))))

(defn- swap-child-left* [parent i]
  (when (get-in parent [:children (dec i)])
    (update parent :children exchange i (dec i))))

(defn- swap-child-right* [parent i]
  (when (get-in parent [:children (inc i)])
    (update parent :children exchange i (inc i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; public API wrapping the above
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn barf-left [loc]
  (-> loc
      (z/edit-parent barf-child-left*)
      (z/child (-> loc :path peek inc))))

(defn barf-right [loc]
  (-> loc
      (z/edit-parent barf-child-right*)
      (z/child (-> loc :path peek))))

(defn delete-sexp [loc]
  (if (p/placeholder-node? (z/node loc))
      (z/remove loc)
      (z/replace loc p/placeholder)))

(defn expand-sexp [loc]
  (z/edit loc p/expand-sexp))

(defn- find-placeholder [direction loc]
  (let [p-locs (z/find loc (comp p/placeholder-node? z/node) direction)
        p-loc  (first p-locs)]
    (if (= p-loc loc) (second p-locs) p-loc)))

(def find-placeholder-left  (partial find-placeholder z/backward))
(def find-placeholder-right (partial find-placeholder z/forward))

(defn join-left [loc]
  (-> loc
      (z/edit-parent join-child-left*)
      (z/child (-> loc :path peek dec))))

(defn join-right [loc]
  (-> loc
      (z/edit-parent join-child-right*)
      (z/child (-> loc :path peek))))

(defn make-curly [loc]
  (z/edit loc make-sexp-type* :map))

(defn make-round [loc]
  (z/edit loc make-sexp-type* :seq))

(defn make-square [loc]
  (z/edit loc make-sexp-type* :vec))

(defn raise-sexp [loc]
  (z/edit-parent loc raise-child*))

(defn slurp-left [loc]
  (-> loc
      (z/edit-parent slurp-child-left*)
      (z/child (max (-> loc :path peek dec) 0))))

(defn slurp-right [loc]
  (-> loc
      (z/edit-parent slurp-child-right*)
      (z/child (-> loc :path peek))))

(defn splice-sexp [loc]
  (-> loc
      (z/edit-parent splice-child*)
      (z/child (-> loc :path peek))))

(defn split-left [loc]
  (let [parent-loc (z/up loc)]
    (-> parent-loc
        (z/edit-parent (partial split-child-left* (-> loc :path peek)))
        (z/child (-> parent-loc :path peek inc))
        z/down)))

(defn split-right [loc]
  (let [parent-loc (z/up loc)]
    (-> parent-loc
        (z/edit-parent (partial split-child-right* (-> loc :path peek)))
        (z/child (-> parent-loc :path peek))
        z/down z/rightmost)))

(defn swap-left [loc]
  (-> loc
      (z/edit-parent swap-child-left*)
      (z/child (-> loc :path peek dec))))

(defn swap-right [loc]
  (-> loc
      (z/edit-parent swap-child-right*)
      (z/child (-> loc :path peek inc))))

(defn toggle-dispatch [loc]
  (z/edit loc toggle-dispatch*))

(defn wrap-curly [loc]
  (-> loc (z/edit wrap-sexp-type* :map) z/down))

(defn wrap-quote [loc]
  (-> loc (z/edit wrap-quote*) z/down))

(defn wrap-round [loc]
  (-> loc (z/edit wrap-sexp-type* :seq) z/down))

(defn wrap-square [loc]
  (-> loc (z/edit wrap-sexp-type* :vec) z/down))
