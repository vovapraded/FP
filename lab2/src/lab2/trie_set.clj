(ns lab2.trie-set
  (:require [lab2.node :as node]
            [lab2.basic :as basic]
            [lab2.combining :as combining]
            [lab2.functional :as functional])
  (:import (clojure.lang IPersistentSet IPersistentCollection Seqable Counted IFn IReduce IReduceInit IKVReduce ISeq)))

(deftype TrieSet [root-node]
  IPersistentSet
  (contains [this key]
    (basic/trie-contains? root-node (str key)))
  
  (disjoin [this key]
    (let [new-root (basic/trie-remove root-node (str key))]
      (if (identical? root-node new-root)
        this
        (TrieSet. new-root))))
  
  (get [this key]
    (when (basic/trie-contains? root-node (str key))
      (str key)))

  IPersistentCollection
  (cons [this key]
    (let [new-root (basic/trie-insert root-node (str key))]
      (if (identical? root-node new-root)
        this
        (TrieSet. new-root))))
  
  (count [this]
    (basic/trie-size root-node))
  
  (empty [this]
    (TrieSet. node/empty-node))
  
  (equiv [this other]
    (and (instance? TrieSet other)
         (= (basic/trie-size root-node) (basic/trie-size (.root-node other)))
         (every? #(basic/trie-contains? (.root-node other) %) (basic/trie-to-seq root-node))))

  Seqable
  (seq [this]
    (when-not (basic/trie-empty? root-node)
      (basic/trie-to-seq root-node)))

  IFn
  (invoke [this key]
    (basic/trie-contains? root-node (str key)))

  IReduce
  (reduce [this f]
    (let [s (seq this)]
      (if s
        (reduce f s)
        (f))))

  IReduceInit
  (reduce [this f init]
    (functional/trie-fold f init root-node))

  IKVReduce
  (kvreduce [this f init]
    ;; Для Set, key и value одинаковые
    (functional/trie-fold (fn [acc word] (f acc word word)) init root-node))

  Object
  (toString [this]
    (str "#{" (clojure.string/join " " (map pr-str (basic/trie-to-seq root-node))) "}"))
  
  (equals [this other]
    (and (instance? TrieSet other)
         (.equiv this other)))
  
  (hashCode [this]
    (reduce + (map hash (basic/trie-to-seq root-node)))))

;; Конструкторы
(defn ->TrieSet
  "Конструктор TrieSet"
  [root-node]
  (TrieSet. root-node))

(defn trie-set
  "Создает новый TrieSet из переданных элементов"
  [& elements]
  (reduce conj (TrieSet. node/empty-node) elements))

(defn from-trie-node
  "Создает TrieSet из существующего TrieNode"
  [node]
  (TrieSet. node))

;; Дополнительные операции для совместимости с существующим API
(defn union
  "Объединение двух TrieSet"
  [set1 set2]
  (TrieSet. (combining/trie-union (.root-node set1) (.root-node set2))))

(defn union-all
  "Объединение множества TrieSet"
  [& sets]
  (if (empty? sets)
    (trie-set)
    (reduce union sets)))

;; Преобразования
(defn to-trie-node
  "Извлекает TrieNode из TrieSet"
  [trie-set]
  (.root-node trie-set))

(defn from-seq
  "Создает TrieSet из последовательности"
  [coll]
  (reduce conj (trie-set) coll))

;; Функциональные операции, возвращающие TrieSet
(defn filter-set
  "Фильтрует TrieSet по предикату и возвращает новый TrieSet"
  [pred trie-set]
  (let [filtered-node (functional/trie-filter pred (.root-node trie-set))]
    (TrieSet. filtered-node)))

(defn map-set
  "Преобразует элементы TrieSet функцией и возвращает новый TrieSet"
  [f trie-set]
  (let [mapped-node (functional/trie-map f (.root-node trie-set))]
    (TrieSet. mapped-node)))

(defn fold-set
  "Свертка TrieSet - применяет функцию к каждому слову в TrieSet и аккумулирует результат"
  [f init trie-set]
  (functional/trie-fold f init (.root-node trie-set)))