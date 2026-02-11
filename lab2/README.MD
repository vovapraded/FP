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

## Публичный API TrieSet

Все публичные методы TrieSet указаны в файле [`core.clj`](src/lab2/core.clj).
### Конструкторы
- [`trie-set`](src/lab2/core.clj#L9) - Создает TrieSet из переданных строк
- [`from-seq`](src/lab2/core.clj#L15) - Создает TrieSet из последовательности строк

### Операции объединения множеств
- [`trie-set-union`](src/lab2/core.clj#L20) - Объединяет два TrieSet
- [`trie-set-union-all`](src/lab2/core.clj#L24) - Объединяет несколько TrieSet

### Функциональные операции
- [`filter-set`](src/lab2/core.clj#L29) - Фильтрует TrieSet по предикату и возвращает новый TrieSet
- [`map-set`](src/lab2/core.clj#L33) - Преобразует элементы TrieSet функцией и возвращает новый TrieSet
- [`reduce-left-set`](src/lab2/core.clj#L37) - Левая свертка TrieSet - обрабатывает элементы слева направо
- [`reduce-right-set`](src/lab2/core.clj#L41) - Правая свертка TrieSet - обрабатывает элементы справа налево

### Функции преобразования и совместимости
- [`from-trie-node`](src/lab2/core.clj#L46) - Преобразует TrieNode в TrieSet
- [`to-trie-node`](src/lab2/core.clj#L47) - Преобразует TrieSet в TrieNode

### Базовые операции TrieNode (для совместимости)
- [`trie-contains?`](src/lab2/core.clj#L61) - Проверка наличия элемента в TrieNode
- [`trie-insert`](src/lab2/core.clj#L62) - Вставка элемента в TrieNode
- [`trie-remove`](src/lab2/core.clj#L63) - Удаление элемента из TrieNode
- [`trie-size`](src/lab2/core.clj#L64) - Получение размера TrieNode
- [`trie-empty?`](src/lab2/core.clj#L65) - Проверка пустоты TrieNode
- [`trie-to-seq`](src/lab2/core.clj#L66) - Преобразование TrieNode в последовательность

### Операции объединения для TrieNode
- [`trie-union`](src/lab2/core.clj#L69) - Объединение двух TrieNode
- [`trie-union-all`](src/lab2/core.clj#L70) - Объединение множества TrieNode

### Операции сравнения для TrieNode
- [`trie-intersection`](src/lab2/core.clj#L73) - Пересечение двух TrieNode
- [`trie-difference`](src/lab2/core.clj#L74) - Разность двух TrieNode

### Функциональные операции для TrieNode
- [`trie-reduce-left`](src/lab2/core.clj#L77) - Левая свертка TrieNode
- [`trie-reduce-right`](src/lab2/core.clj#L78) - Правая свертка TrieNode
- [`trie-filter`](src/lab2/core.clj#L79) - Фильтрация TrieNode
- [`trie-map`](src/lab2/core.clj#L80) - Отображение функции на TrieNode

## Внутренний API

### Файлы внутренней реализации:
- [`node.clj`](src/lab2/node.clj) - Определяет структуру узлов Trie и вспомогательные функции для работы с ними
- [`basic.clj`](src/lab2/basic.clj) - Базовые операции над узлами Trie (вставка, удаление, поиск, размер)
- [`functional.clj`](src/lab2/functional.clj) - Функциональные операции (свертки, фильтрация, отображение)
- [`combining.clj`](src/lab2/combining.clj) - Операции объединения множеств (union, intersection, difference)
- [`trie_set.clj`](src/lab2/trie_set.clj) - Полиморфная обертка TrieSet, реализующая интерфейсы Clojure
- [`core.clj`](src/lab2/core.clj) - API-фасад библиотеки, объединяющий все публичные функции в единую точку входа


## Тестирование
- Unit-тесты для всех операций
