(ns flense.zip
  (:refer-clojure :exclude [remove replace])
  (:require [flense.util :refer [delete insert update]]))

;; ====================================
;; public API (protocols)
;; ====================================

(defprotocol IZipper
  (down [this])
  (edit [this f args])
  (insert-left [this node])
  (insert-right [this node])
  (left [this])
  (leftmost [this])
  (node [this])
  (remove [this])
  (replace [this node])
  (right [this])
  (rightmost [this])
  (up [this]))

;; ====================================
;; private helpers (path manipulation)
;; ====================================

(defn- sibling* [path n]
  (when (seq path) (conj (pop path) n)))

(defn- down* [path]
  (conj path 0))

(defn- left* [path]
  (when (seq path)
    (let [n (peek path)]
      (when (pos? n) (sibling* path (dec n))))))

(defn leftmost* [path]
  (or (sibling* path 0) path))

(defn- right* [path]
  (when (seq path)
    (sibling* path (inc (peek path)))))

(defn- up* [path]
  (when (seq path) (pop path)))

(defn full-path [path]
  (vec (if (seq path)
           (interleave (repeat :children) path)
           path)))

;; ====================================
;; private helpers (tree manipulation)
;; ====================================

(defn- node* [tree path]
  (get-in tree (full-path path)))

(defn- edit* [tree path f args]
  (let [edit-f (partial update-in tree (full-path path) f)]
    (apply edit-f args)))

(defn- insert-left* [tree path value]
  (if-let [parent-path (up* path)]
    (update-in tree (conj (full-path parent-path) :children)
               insert (peek path) value)
    tree))

(defn- insert-right* [tree path value]
  (if-let [parent-path (up* path)]
    (update-in tree (conj (full-path parent-path) :children)
               insert (inc (peek path)) value)
    tree))

(defn- remove* [tree path]
  (when-let [parent-path (up* path)]
    (let [child-idx    (peek path)
          children-ks  (conj (full-path parent-path) :children)
          old-children (get-in tree children-ks)
          new-children (delete old-children child-idx)]
      (assoc-in tree children-ks new-children))))

(defn- replace* [tree path node]
  (assoc-in tree (full-path path) node))

;; ====================================
;; public API (implementations)
;; ====================================

(defrecord SimpleZipper [path tree]
  IZipper
  (down [this]
    (let [new-path (down* path)]
      (when (node* tree new-path)
        (assoc this :path new-path))))

  (edit [this f args]
    (update this :tree edit* path f args))

  (insert-left [this node]
    (update this :tree insert-left* path node))

  (insert-right [this node]
    (update this :tree insert-right* path node))

  (left [this]
    (when-let [new-path (left* path)]
      (assoc this :path new-path)))

  (leftmost [this]
    (update this :path leftmost*))

  (node [this]
    (node* tree path))

  (remove [this]
    (when-let [new-tree (remove* tree path)]
      (let [new-this  (assoc this :tree new-tree)
            left-path (left* path)
            go-left?  (and left-path (node* new-tree left-path))
            new-path  (if go-left? left-path (up* path))]
        (if new-path
            (assoc new-this :path new-path)
            (throw (js/Error. "unchecked remove at root"))))))

  (replace [this node]
    (update this :tree replace* path node))

  (right [this]
    (let [new-path (right* path)]
      (when (node* tree new-path)
        (assoc this :path new-path))))

  (rightmost [this]
    (if (seq path)
        (let [siblings (:children (node* tree (up* path)))]
          (assoc this :path (sibling* path (dec (count siblings)))))
        this))

  (up [this]
    (when-let [new-path (up* path)]
      (assoc this :path new-path))))

(defrecord BoundedZipper [loc]
  IZipper
  (down [this]
    (assoc this :loc (or (down loc) loc)))

  (edit [this f args]
    (update this :loc edit f args))

  (insert-left [this node]
    (update this :loc insert-left node))

  (insert-right [this node]
    (update this :loc #(-> % (insert-right node) right)))

  (left [this]
    (assoc this :loc (or (left loc) (rightmost loc))))

  (leftmost [this]
    (update this :loc leftmost))

  (node [this]
    (node loc))

  (remove [this]
    (update this :loc remove))

  (replace [this node]
    (update this :loc replace node))

  (right [this]
    (assoc this :loc (or (right loc) (leftmost loc))))

  (rightmost [this]
    (update this :loc rightmost))

  (up [this]
    (assoc this :loc (or (up loc) loc))))
