# Прадед Владимир
# TrieSet - Лабораторная работа №2

## Требования:

1. Функции:
  - добавление и удаление элементов;
  - фильтрация;
  - отображение (map);
  - свертки (левая и правая);
  - структура должна быть [моноидом](https://ru.m.wikipedia.org/wiki/Моноид).
2. Структуры данных должны быть неизменяемыми.
3. Библиотека должна быть протестирована в рамках unit testing.
4. Библиотека должна быть протестирована в рамках property-based тестирования (как минимум 3 свойства, включая свойства моноида).
5. Структура должна быть полиморфной.
6. Требуется использовать идиоматичный для технологии стиль программирования. Примечание: некоторые языки позволяют получить большую часть API через реализацию небольшого интерфейса. Так как лабораторная работа про ФП, а не про экосистему языка -- необходимо реализовать их вручную и по возможности -- обеспечить совместимость.
7. Обратите внимание:
  - API должно быть реализовано для заданного интерфейса и оно не должно "протекать". На уровне тестов -- в первую очередь нужно протестировать именно API (dict, set, bag).
  - Должна быть эффективная реализация функции сравнения (не наивное приведение к спискам, их сортировка с последующим сравнением), реализованная на уровне API, а не внутреннего представления.

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

### Операции над множествами
```clojure
;; combining.clj
(defn trie-union [set1 set2])           ; объединение множеств
(defn trie-union-all [& tries])         ; объединение нескольких множеств
(defn trie-intersection [trie1 trie2])  ; пересечение множеств
(defn trie-difference [trie1 trie2])    ; разность множеств (trie1 - trie2)
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
(trie-intersection ts1 ts2)          ; пересечение на уровне узлов
(trie-difference ts1 ts2)            ; разность на уровне узлов
```

## Операции над множествами

### Пересечение (Intersection)
Возвращает новое множество, содержащее только элементы, присутствующие в обоих исходных множествах.

```clojure
(let [ts1 (trie-set "cat" "car" "card")
      ts2 (trie-set "cat" "dog" "card")
      result (trie-intersection (to-trie-node ts1) (to-trie-node ts2))]
  (trie-to-seq result))  ; => ("cat" "card")
```

### Разность (Difference)
Возвращает новое множество, содержащее элементы из первого множества, которых нет во втором.

```clojure
(let [ts1 (trie-set "cat" "car" "card")
      ts2 (trie-set "cat" "dog")
      result (trie-difference (to-trie-node ts1) (to-trie-node ts2))]
  (trie-to-seq result))  ; => ("car" "card")
```

## Тестирование
- Unit-тесты для всех операций
- Property-based тесты моноидальных свойств:
  - Ассоциативность: `(union (union a b) c) = (union a (union b c))`
  - Нейтральный элемент: `(union empty-set a) = a`
  - Идемпотентность: `(union a a) = a`