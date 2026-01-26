(ns lab2.combining
  (:require [clojure.set :as set]
            [lab2.node :as node :refer [->TrieNode]]
            [lab2.basic :as basic]
            [lab2.functional :as functional]))

(defn trie-set 
  [& words]
  (reduce basic/trie-insert node/empty-node words))

(defn trie-union [set1 set2]
  (functional/trie-reduce-left basic/trie-insert set1 set2))


(defn trie-union-all
  [& tries]
  (reduce trie-union node/empty-node tries))