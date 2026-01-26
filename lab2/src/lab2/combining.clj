(ns lab2.combining
  (:require [lab2.basic :as basic]
            [lab2.functional :as functional]
            [lab2.node :as node]))

(defn trie-set
  [& words]
  (reduce basic/trie-insert node/empty-node words))

(defn trie-union [set1 set2]
  (functional/trie-reduce-left basic/trie-insert set1 set2))


(defn trie-union-all
  [& tries]
  (reduce trie-union node/empty-node tries))