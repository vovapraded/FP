(ns lab2.core)

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
  (loop [current-node node
         ch (first word)
         rest-word (rest word)
         ]
    (if (empty? rest-word)
      (:terminal? current-node)
      (if (contains? (:children current-node) ch)
        (recur (get (:children current-node) ch) (first rest-word) (rest rest-word))
        false
        )
      )
    )
  )


(defn trie-insert [node word]
  (if (empty? word)
    (if (:terminal? node)
      node  ; слово уже есть
      (-> node
          (assoc :terminal? true)
          (update-count 1)))  ; новое слово

    (let [ch (first word)
          rest-word (rest word)
          child-node (get-or-create-child node ch)
          updated-child (trie-insert child-node rest-word)
          count-delta (- (:count updated-child) (:count child-node))]

      (-> node
          (assoc-in [:children ch] updated-child)
          (update-count count-delta)))))


;; Удаление строки
(defn trie-remove [node word]
  (if (empty? word)
    (if (:terminal? node)
      (-> node
          (assoc :terminal? false)
          (update-count -1))
      node)  ; Слова и так нет

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



;; Получение размера множества
(defn trie-size [node]
  (:count node))

;; Проверка на пустоту
(defn trie-empty? [node]
  (= (:count node) 0))

;; Получение всех слов в виде последовательности
;(defn trie-to-seq [node]
;  (letfn [(collect [current-node prefix]
;            (let [words (if (:terminal? current-node) [prefix] [])]
;              (concat words
;                      (mapcat (fn [[ch child-node]]
;                                (collect child-node (str prefix ch)))
;                              (:children current-node)))))]
;    (collect node "")))


;; Создание trie-множества из слов
(defn trie-set [& words]
  (reduce trie-insert empty-node words))

;;; Функция фильтрации
;(defn trie-filter
;  "Фильтрует элементы trie-множества по предикату, возвращает новое множество"
;  [pred trie]
;  (let [all-words (trie-to-seq trie)
;        filtered-words (filter pred all-words)]
;    (reduce insert empty-node filtered-words)))
;
;;; Функция отображения
;(defn trie-map
;  "Применяет функцию к каждому элементу trie-множества, возвращает новое множество"
;  [f trie]
;  (let [all-words (trie-to-seq trie)
;        mapped-words (map f all-words)]
;    (reduce insert empty-node mapped-words)))
;
;;; Левая свертка - обходим trie слева направо напрямую
;(defn trie-fold-left
;  "Левая свертка над элементами trie-множества"
;  [f acc trie]
;  (letfn [(fold-helper [current-node prefix acc]
;            (let [acc' (if (:terminal? current-node)
;                        (f acc prefix)
;                        acc)]
;              (reduce (fn [acc [ch child-node]]
;                        (fold-helper child-node (str prefix ch) acc))
;                      acc'
;                      (sort (:children current-node)))))]
;    (fold-helper trie "" acc)))
;
;;; Правая свертка - обходим trie справа налево напрямую
;(defn trie-fold-right
;  "Правая свертка над элементами trie-множества"
;  [f acc trie]
;  (letfn [(fold-helper [current-node prefix acc]
;            (let [children-result (reduce (fn [acc [ch child-node]]
;                                           (fold-helper child-node (str prefix ch) acc))
;                                         acc
;                                         (reverse (sort (:children current-node))))]
;              (if (:terminal? current-node)
;                (f prefix children-result)
;                children-result)))]
;    (fold-helper trie "" acc)))


;; Примеры использования
(comment
  ;; Создание trie-множества
  (def trie (trie-set "cat" "car" "cart" "dog"))
  
  ;; Основные операции
  (trie-contains? trie "cat")    ; → true
  (trie-size trie)               ; → 4
  (trie-to-seq trie)            ; → ("car" "cart" "cat" "dog")
  
  ;; Добавление и удаление элементов
  (def new-trie (trie-insert trie "bird"))
  (trie-size new-trie)          ; → 5
  
  (def smaller-trie (trie-remove trie "cat"))
  (trie-size smaller-trie)      ; → 3
  
  ;; Фильтрация - оставляем только слова длиннее 3 символов
  (def filtered (trie-filter #(> (count %) 3) trie))
  (trie-to-seq filtered)        ; → ("cart")
  
  ;; Отображение - преобразуем все слова в верхний регистр
  (def mapped (trie-map clojure.string/upper-case trie))
  (trie-to-seq mapped)          ; → ("CAR" "CART" "CAT" "DOG")
  
  ;; Левая свертка - соединяем все слова через запятую
  (trie-fold-left #(if (empty? %1) %2 (str %1 ", " %2)) "" trie)
  ; → "car, cart, cat, dog"
  
  ;; Правая свертка - подсчитываем общую длину всех слов
  (trie-fold-right #(+ %1 (count %2)) 0 trie)
  ; → 13 (3+4+3+3)
  
  ;; Комбинированные операции
  ;; Найдем длину самого длинного слова, начинающегося на "ca"
  (-> trie
      (trie-filter #(.startsWith % "ca"))
      (trie-fold-left #(max %1 (count %2)) 0))
  ; → 4 ("cart")
)

