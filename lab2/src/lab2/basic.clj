(ns lab2.basic
  (:require [lab2.node :as node]))

(defn trie-contains?
  "Проверяет, содержит ли trie заданное слово"
  [node word]
  (if (empty? word)
    (:terminal? node)
    (let [ch (first word)
          rest-word (rest word)
          child (get (:children node) ch)]
      (if child
        (trie-contains? child rest-word)
        false))))

(defn trie-insert
  "Вставляет слово в trie"
  [node word]
  (if (empty? word)
    (if (:terminal? node)
      node                                                  ; слово уже есть
      (-> node
          (assoc :terminal? true)
          (node/update-count 1)))                           ; новое слово

    (let [ch (first word)
          rest-word (rest word)
          child-node (node/get-or-create-child node ch)
          updated-child (trie-insert child-node rest-word)
          count-delta (- (:count updated-child) (:count child-node))]

      (-> node
          (assoc-in [:children ch] updated-child)
          (node/update-count count-delta)))))

(defn trie-remove
  "Удаляет слово из trie"
  [node word]
  (if (empty? word)
    (if (:terminal? node)
      (-> node
          (assoc :terminal? false)
          (node/update-count -1))
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
              (node/update-count delta)))))))

(defn trie-size
  "Возвращает количество слов в trie"
  [node]
  (:count node))

(defn trie-empty?
  "Проверяет, пуст ли trie"
  [node]
  (= (:count node) 0))

(defn trie-to-seq
  "Преобразует trie в последовательность слов"
  [node]
  (letfn [(collect [current-node prefix]
            (->> (:children current-node)
                 (mapcat (fn [[ch child-node]]
                           (collect child-node (str prefix ch))))
                 (concat (when (:terminal? current-node) [prefix]))))]
    (collect node "")))