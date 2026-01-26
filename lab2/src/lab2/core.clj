(ns lab2.core
  (:require [lab2.basic :as basic]
            [lab2.combining :as combining]
            [lab2.functional :as functional]
            [lab2.node :as node]
            [lab2.trie-set :as trie-set]))

;; Конструкторы TrieSet
(def trie-set
  "Создает TrieSet из переданных строк.
   Пример: (trie-set \"cat\" \"car\" \"card\")
   Без аргументов создает пустое множество: (trie-set)"
  trie-set/trie-set)

(def from-seq
  "Создает TrieSet из последовательности строк"
  trie-set/from-seq)

;; Операции над TrieSet
(def trie-set-union
  "Объединяет два TrieSet"
  trie-set/union)

(def trie-set-union-all
  "Объединяет несколько TrieSet"
  trie-set/union-all)

;; Функциональные операции, возвращающие TrieSet
(def filter-set
  "Фильтрует TrieSet по предикату и возвращает новый TrieSet"
  trie-set/filter-set)

(def map-set
  "Преобразует элементы TrieSet функцией и возвращает новый TrieSet"
  trie-set/map-set)

(def reduce-left-set
  "Левая свертка TrieSet - обрабатывает элементы слева направо"
  trie-set/reduce-left-set)

(def reduce-right-set
  "Правая свертка TrieSet - обрабатывает элементы справа налево"
  trie-set/reduce-right-set)

;; Конструктор для внутреннего использования
(def ->TrieSet trie-set/->TrieSet)

;; Функции преобразования между TrieSet и TrieNode (для совместимости)
(def from-trie-node trie-set/from-trie-node)
(def to-trie-node trie-set/to-trie-node)

;; =============================================================================
;; TrieNode - низкоуровневые операции
;; =============================================================================

;; Базовые функции узлов (для внутреннего использования)
(def make-node node/make-node)
(def empty-node node/empty-node)
(def get-or-create-child node/get-or-create-child)
(def get-child node/get-child)
(def update-count node/update-count)

;; Базовые операции над TrieNode (для совместимости с существующим кодом)
(def trie-contains? basic/trie-contains?)
(def trie-insert basic/trie-insert)
(def trie-remove basic/trie-remove)
(def trie-size basic/trie-size)
(def trie-empty? basic/trie-empty?)
(def trie-to-seq basic/trie-to-seq)

;; Операции объединения для TrieNode
(def trie-union combining/trie-union)
(def trie-union-all combining/trie-union-all)

;; Операции сравнения для TrieNode
(def trie-intersection combining/trie-intersection)
(def trie-difference combining/trie-difference)

;; Функциональные операции для TrieNode
(def trie-reduce-left functional/trie-reduce-left)
(def trie-reduce-right functional/trie-reduce-right)
(def trie-filter functional/trie-filter)
(def trie-map functional/trie-map)
