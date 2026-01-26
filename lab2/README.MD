# TrieSet - Функциональная лабораторная работа №2

## Требования к разработанному ПО

- **Структура данных**: Неизменяемое множество строк на основе префиксного дерева (Trie)
- **Операции**: добавление/удаление элементов, фильтрация, отображение (map), свертки (левая/правая)
- **Моноид**: структура образует моноид с операцией объединения множеств
- **Полиморфизм**: реализация через протоколы Clojure (IPersistentSet, IPersistentCollection, IReduce, Seqable)
- **Неизменяемость**: все операции возвращают новые экземпляры
- **Тестирование**: unit-тесты и property-based тестирование с проверкой свойств моноида

## Ключевые элементы реализации

### Структура узла дерева
```clojure
;; node.clj
(defrecord TrieNode [children terminal? count])
```

### Базовые операции
```clojure
;; basic.clj
(defn trie-insert [node word])      ; вставка слова
(defn trie-remove [node word])      ; удаление слова
(defn trie-contains? [node word])   ; проверка вхождения
```

### Функциональные операции
```clojure
;; functional.clj
(defn trie-reduce-left [f init trie])   ; левая свертка
(defn trie-reduce-right [f init trie])  ; правая свертка
(defn trie-filter [pred trie])          ; фильтрация
(defn trie-map [f trie])               ; отображение
```

### Моноид
```clojure
;; combining.clj
(defn trie-union [set1 set2])        ; бинарная операция
(defn trie-union-all [& tries])      ; объединение множеств
```

### Полиморфная обертка
```clojure
;; trie_set.clj
(deftype TrieSet [root-node]
  IPersistentSet
  IPersistentCollection  
  IReduce
  Seqable)
```

### API
```clojure
;; core.clj - публичные функции
(trie-set "cat" "car" "card")        ; конструктор
(conj ts "new-word")                 ; добавление
(disj ts "word")                     ; удаление
(filter-set pred ts)                 ; фильтрация
(map-set f ts)                       ; отображение
(trie-set-union ts1 ts2)            ; объединение
```

## Тестирование
- Unit-тесты для всех операций
- Property-based тесты моноидальных свойств:
  - Ассоциативность: `(union (union a b) c) = (union a (union b c))`
  - Нейтральный элемент: `(union empty-set a) = a`
  - Идемпотентность: `(union a a) = a`