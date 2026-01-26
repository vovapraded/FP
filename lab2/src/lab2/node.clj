(ns lab2.node)

;; Узел дерева
(defrecord TrieNode [children terminal? node-count])

(defn make-node
  "Создание нового узла trie"
  ([] (->TrieNode {} false 0))
  ([terminal?] (->TrieNode {} terminal? (if terminal? 1 0))))

(def empty-node
  "Пустой узел trie"
  (make-node))

(defn get-or-create-child
  "Получить дочерний узел или создать новый если не существует"
  [node ch]
  (get (:children node) ch (make-node)))

(defn get-child
  "Получить дочерний узел"
  [node ch]
  (get (:children node) ch))

(defn update-count
  "Обновить счетчик узла на заданную дельту"
  [node delta]
  (update node :node-count + delta))