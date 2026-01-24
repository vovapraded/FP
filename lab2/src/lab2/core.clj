(ns lab2.core
  (:require [lab2.node :as node]
            [lab2.basic :as basic]
            [lab2.combining :as combining]))

;; Экспорт базовых функций узлов
(def make-node node/make-node)
(def empty-node node/empty-node)
(def get-or-create-child node/get-or-create-child)
(def get-child node/get-child)
(def update-count node/update-count)

;; Экспорт базовых операций
(def trie-contains? basic/trie-contains?)
(def trie-insert basic/trie-insert)
(def trie-remove basic/trie-remove)
(def trie-size basic/trie-size)
(def trie-empty? basic/trie-empty?)
(def trie-to-seq basic/trie-to-seq)

;; Экспорт операций объединения
(def trie-set combining/trie-set)
(def trie-union combining/trie-union)
(def trie-union-all combining/trie-union-all)
