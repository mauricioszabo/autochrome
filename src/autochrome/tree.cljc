(ns autochrome.tree
  (:require [autochrome.common :as clj-common]))

(defn branch?
  [{:keys [type] :as form}]
  (or (clj-common/decoration? form)
      (= type :coll)
      (= type :lambda)
      (= type :data-reader)
      (= type :reader-conditional)
      (= type :reader-conditional-splicing)
      (= type :quote)))

(defn ->children
  [{:keys [type] :as form}]
  ;; The parser is unfortunately designed so that recursing into everything
  ;; is a little bit... involved.
  (cond
    (= type :quote) (:val form)
    (= type :lambda) (:contents (:text form))
    :else (:contents form)))

(defn put-sizes
  [szmap form]
  (let [children (->children form)
        size (if-not children
               (if-let [t (:text form)]
                 (count t)
                 ;; empty collection
                 1)
               (let [tcost (volatile! 0)]
                 (loop [i 0
                        [c & cs] children]
                   (when c
                     (vswap! tcost + (put-sizes szmap c))
                     (recur (inc i) cs)))
                 @tcost))]
    (.put szmap form size)
    size))

(defn put-hashes
  [hmap form]
  (let [children (->children form)
        size (if-not children
               (if-let [t (:text form)]
                 (clj-common/hash-combine (.hashCode t) (.hashCode (:type form)))
                 (if (= :coll (:type form))
                   ;; empty collection
                   (.hashCode (:delim form))
                   (if (nil? form)
                     0
                     (throw (ex-info "unhashable" {:form form})))))
               (let [type-hash (clj-common/hash-combine (.hashCode (:type form)) (hash (:delim form)))
                     parent-hash (volatile! (clj-common/hash-combine 0x1a814d0 type-hash))]
                 (loop [i 0
                        [c & cs] children]
                   (when c
                     (vreset! parent-hash (clj-common/hash-combine @parent-hash (put-hashes hmap c)))
                     (recur (inc i) cs)))
                 @parent-hash))]
    (.put hmap form size)
    size))
