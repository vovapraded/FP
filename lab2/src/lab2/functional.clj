(ns lab2.functional
  (:require [lab2.node :as node]
            [lab2.basic :as basic]))

(defn trie-fold
  "Свертка trie - применяет функцию к каждому слову в trie и аккумулирует результат.
   f - функция, принимающая аккумулятор и слово
   init - начальное значение аккумулятора
   trie - дерево для свертки"
  [f init trie]
  (letfn [(fold-impl [acc current-node prefix]
            (let [acc-after-current (if (:terminal? current-node)
                                      (f acc prefix)
                                      acc)]
              (->> (:children current-node)
                   (reduce (fn [current-acc [ch child-node]]
                             (fold-impl current-acc child-node (str prefix ch)))
                           acc-after-current))))]
    (fold-impl init trie "")))



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
