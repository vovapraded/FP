(ns lab2.functional
  (:require [lab2.node :as node]
            [lab2.basic :as basic]))

(defn trie-reduce-left
  "Левая свертка trie - применяет функцию к каждому слову в порядке обхода дерева.
   f - функция (acc word) -> new-acc, принимающая аккумулятор СЛЕВА
   init - начальное значение аккумулятора
   trie - дерево для свертки"
  [f init trie]
  (letfn [(reduce-impl [acc current-node prefix]
            ;; Сначала обрабатываем текущий узел (если он терминальный)
            (let [acc-after-current (if (:terminal? current-node)
                                      (f acc prefix)
                                      acc)]
              ;; Затем рекурсивно обрабатываем детей СЛЕВА НАПРАВО
              (reduce (fn [current-acc [ch child-node]]
                        (reduce-impl current-acc child-node (str prefix ch)))
                      acc-after-current
                      (:children current-node))))]
    (reduce-impl init trie "")))


(defn trie-reduce-right [f init trie]
  (letfn [(reduce-right-impl [current-node prefix]
            (let [child-results (->> (:children current-node)
                                     (map (fn [[ch child-node]]
                                            (reduce-right-impl child-node (str prefix ch))))
                                     (reverse)) ; Обрабатываем детей справа налево

                  current-result (if (:terminal? current-node)
                                   (f prefix (reduce #(f %2 %1) init child-results))
                                   (reduce #(f %2 %1) init child-results))]
              current-result))]
    (reduce-right-impl trie "")))


;; Алиас для обратной совместимости
(def trie-fold trie-reduce-left)

(defn trie-filter [pred trie]
  (letfn [(filter-nodes [node prefix]
            (let [keep-word? (and (:terminal? node) (pred prefix))

                  [new-children children-count]
                  (->> (:children node)
                       (reduce (fn [[children-acc count-acc] [ch child]]
                                 (let [filtered-child (filter-nodes child (str prefix ch))]
                                   (if (basic/trie-empty? filtered-child)
                                     [children-acc count-acc]
                                     [(assoc children-acc ch filtered-child)
                                      (+ count-acc (:count filtered-child))])))
                               [{} 0]))

                  new-count (+ children-count (if keep-word? 1 0))]

              (node/->TrieNode new-children keep-word? new-count)))]

    (filter-nodes trie "")))

(defn trie-map [f trie]
  (trie-fold (fn [acc word]
               (basic/trie-insert acc (f word)))
             node/empty-node
             trie))
