(ns lab2.core)

;; Узел дерева
(defrecord TrieNode [children terminal?])

;; Пустой узел
(def empty-node (->TrieNode {} false))

;; Добавление строки
(defn insert [node word]
  (if (empty? word)
    (assoc node :terminal? true)
    (let [ch (first word)
          rest-word (rest word)
          child-node (get (:children node) ch empty-node)
          updated-child (insert child-node rest-word)]
      (assoc node :children
                  (assoc (:children node) ch updated-child)))))

;; Проверка наличия строки
(defn trie-contains? [node word]
  (loop [current node
         chars word]
    (cond
      (nil? current) false
      (empty? chars) (:terminal? current)
      :else (recur (get (:children current) (first chars))
                   (rest chars)))))

;; Само множество
(defn trie-set [& words]
  (reduce insert empty-node words))

;; Примеры использования
;; (def s (trie-set "cat" "car" "cart" "dog"))

;; (trie-contains? s "cat")   ; → true
;; (trie-contains? s "ca")    ; → false
;; (trie-contains? s "cart")  ; → true
;; (trie-contains? s "camel") ; → false
