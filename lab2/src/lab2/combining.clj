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

;; Операции сравнения множеств
(defn trie-intersection
  "Возвращает пересечение двух trie-множеств."
  [trie1 trie2]
  (letfn [(intersect-nodes [node1 node2]
            (let [;; Узел терминальный, только если оба исходных терминальные
                  is-terminal? (and (:terminal? node1) (:terminal? node2))

                  ;; Пересекаем только общие дочерние узлы
                  [new-children total-count]
                  (->> (:children node1)
                       (keep (fn [[ch child1]]
                               (when-let [child2 (get (:children node2) ch)]
                                 [ch (intersect-nodes child1 child2)])))
                       (filter (fn [[_ child-node]] (pos? (:node-count child-node))))
                       (reduce (fn [[children-acc count-acc] [ch child]]
                                 [(assoc children-acc ch child)
                                  (+ count-acc (:node-count child))])
                               [{} 0]))

                  final-count (+ total-count (if is-terminal? 1 0))]

              (node/->TrieNode new-children is-terminal? final-count)))]
    (intersect-nodes trie1 trie2)))

(defn trie-difference
  "Возвращает разность trie1 - trie2 (элементы из trie1, которых нет в trie2)."
  [trie1 trie2]
  (letfn [(diff-nodes [node1 node2]
            (let [;; Узел терминальный только если он есть в первом, но не во втором
                  is-terminal? (and (:terminal? node1)
                                    (not (:terminal? node2)))

                  ;; Обрабатываем дочерние узлы
                  [new-children total-count]
                  (->> (:children node1)
                       (map (fn [[ch child1]]
                              [ch (if-let [child2 (get (:children node2) ch)]
                                    (diff-nodes child1 child2)
                                    ; Если узла нет во втором trie, берем весь поддерево
                                    child1)]))
                       (filter (fn [[_ child-node]] (pos? (:node-count child-node))))
                       (reduce (fn [[children-acc count-acc] [ch child]]
                                 [(assoc children-acc ch child)
                                  (+ count-acc (:node-count child))])
                               [{} 0]))

                  final-count (+ total-count (if is-terminal? 1 0))]

              (node/->TrieNode new-children is-terminal? final-count)))]
    (diff-nodes trie1 trie2)))