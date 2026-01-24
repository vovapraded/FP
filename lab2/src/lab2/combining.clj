(ns lab2.combining
  (:require [clojure.set :as set]
            [lab2.node :as node :refer [->TrieNode]]
            [lab2.basic :as basic]))

(defn trie-set 
  "Создает trie-множество из слов"
  [& words]
  (reduce basic/trie-insert node/empty-node words))

(defn trie-union 
  "Объединяет два trie в одно"
  [trie1 trie2]
  (cond
    (basic/trie-empty? trie1) trie2
    (basic/trie-empty? trie2) trie1
    :else (letfn [(children-union [children1 children2]
                    (->> (set/union (set (keys children1))
                                    (set (keys children2)))
                         (reduce (fn [acc ch]
                                   (let [child1 (get children1 ch)
                                         child2 (get children2 ch)]
                                     (cond
                                       (and child1 child2) (assoc acc ch (trie-union child1 child2))
                                       child1 (assoc acc ch child1)
                                       child2 (assoc acc ch child2))))
                                 {})))]

            (let [merged-children (children-union (:children trie1) (:children trie2))
                  terminal? (or (:terminal? trie1) (:terminal? trie2))
                  children-count (reduce + (map :count (vals merged-children)))
                  total-count (if terminal? (inc children-count) children-count)]

              (->TrieNode merged-children terminal? total-count)))))

(defn trie-concat 
  "Объединяет несколько trie в одно"
  [& tries]
  (reduce trie-union node/empty-node tries))