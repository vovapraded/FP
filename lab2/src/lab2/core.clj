(ns lab2.core
  (:require [clojure.set :as set]))

;; Узел дерева
(defrecord TrieNode [children terminal? count])

(defn make-node
  ([] (->TrieNode {} false 0))
  ([terminal?] (->TrieNode {} terminal? (if terminal? 1 0))))

(def empty-node (make-node))

(defn get-or-create-child [node ch]
  (get (:children node) ch (make-node)))

(defn get-child [node ch]
  (get (:children node) ch))

(defn update-count [node delta]
  (update node :count + delta))

(defn trie-contains? [node word]
  (if (empty? word)
    (:terminal? node)
    (let [ch (first word)
          rest-word (rest word)
          child (get (:children node) ch)]
      (if child
        (trie-contains? child rest-word)
        false))))


(defn trie-insert [node word]
  (if (empty? word)
    (if (:terminal? node)
      node                                                  ; слово уже есть
      (-> node
          (assoc :terminal? true)
          (update-count 1)))                                ; новое слово

    (let [ch (first word)
          rest-word (rest word)
          child-node (get-or-create-child node ch)
          updated-child (trie-insert child-node rest-word)
          count-delta (- (:count updated-child) (:count child-node))]

      (-> node
          (assoc-in [:children ch] updated-child)
          (update-count count-delta)))))


(defn trie-remove [node word]
  (if (empty? word)
    (if (:terminal? node)
      (-> node
          (assoc :terminal? false)
          (update-count -1))
      node)                                                 ; Слова и так нет

    (let [ch (first word)
          rest-word (rest word)
          child (get (:children node) ch)]

      (if-not child
        node

        (let [updated-child (trie-remove child rest-word)
              delta (- (:count updated-child) (:count child))]
          (-> node
              (assoc :children (if (zero? (:count updated-child))
                                 (dissoc (:children node) ch)
                                 (assoc (:children node) ch updated-child)))
              (update-count delta)))))))



(defn trie-size [node]
  (:count node))

(defn trie-empty? [node]
  (= (:count node) 0))

(defn trie-to-seq [node]
  (letfn [(collect [current-node prefix]
            (->> (:children current-node)
                 (mapcat (fn [[ch child-node]]
                           (collect child-node (str prefix ch))))
                 (concat (when (:terminal? current-node) [prefix]))))]
    (collect node "")))


;; Создание trie-множества из слов
(defn trie-set [& words]
  (reduce trie-insert empty-node words))


(defn trie-union [trie1 trie2]
  (cond
    (trie-empty? trie1) trie2
    (trie-empty? trie2) trie1
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


(defn trie-concat [& tries]
  (reduce trie-union empty-node tries))

