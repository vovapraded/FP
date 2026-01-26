(ns lab2.functional-test
  (:require [clojure.test :refer :all]
            [lab2.core :refer :all]))

(deftest trie-set-reduce-test
  (testing "Reduce пустого множества"
    (let [ts (trie-set)]
      (is (= 0 (reduce (fn [acc _word] (inc acc)) 0 ts)))
      (is (= [] (reduce conj [] ts)))))

  (testing "Reduce с одним словом"
    (let [ts (trie-set "cat")]
      (is (= 1 (reduce (fn [acc _word] (inc acc)) 0 ts)))
      (is (= ["cat"] (reduce conj [] ts)))))

  (testing "Reduce с несколькими словами"
    (let [ts (trie-set "cat" "car" "card" "care")]
      ;; Подсчет количества слов
      (is (= 4 (reduce (fn [acc _word] (inc acc)) 0 ts)))
      
      ;; Сбор всех слов в список
      (let [words (reduce conj [] ts)]
        (is (= 4 (count words)))
        (is (every? #(contains? ts %) words))
        (is (contains? (set words) "cat"))
        (is (contains? (set words) "car"))
        (is (contains? (set words) "card"))
        (is (contains? (set words) "care")))))

  (testing "Reduce с суммированием длин слов"
    (let [ts (trie-set "a" "ab" "abc")]
      (is (= 6 (reduce (fn [acc word] (+ acc (count word))) 0 ts)))))

  (testing "Reduce с конкатенацией строк"
    (let [ts (trie-set "hello" "world")]
      (let [result (reduce str "" ts)]
        (is (= 10 (count result))))))

  (testing "Reduce с фильтрацией в процессе"
    (let [ts (trie-set "cat" "car" "dog" "duck")]
      ;; Считаем только слова, начинающиеся с 'c'
      (is (= 2 (reduce (fn [acc word]
                        (if (.startsWith word "c")
                          (inc acc)
                          acc))
                       0 ts)))))

  (testing "Reduce с пустой строкой в множестве"
    (let [ts (trie-set "" "cat")]
      (is (= 2 (reduce (fn [acc _word] (inc acc)) 0 ts)))
      (let [words (set (reduce conj [] ts))]
        (is (contains? words ""))
        (is (contains? words "cat"))))))

(deftest trie-set-collection-operations-test
  (testing "Фильтрация пустого множества"
    (let [ts (trie-set)
          result (filter (constantly true) ts)]
      (is (empty? result))))

  (testing "Фильтрация с предикатом true"
    (let [ts (trie-set "cat" "car" "dog")
          result (filter (constantly true) ts)]
      (is (= 3 (count result)))
      (is (= #{"cat" "car" "dog"} (set result)))))

  (testing "Фильтрация с предикатом false"
    (let [ts (trie-set "cat" "car" "dog")
          result (filter (constantly false) ts)]
      (is (empty? result))))

  (testing "Фильтрация по длине слова"
    (let [ts (trie-set "a" "ab" "abc" "abcd")
          result (filter #(>= (count %) 3) ts)]
      (is (= 2 (count result)))
      (is (= #{"abc" "abcd"} (set result)))))

  (testing "Фильтрация по началу слова"
    (let [ts (trie-set "cat" "car" "card" "dog" "duck")
          result (filter #(.startsWith % "ca") ts)]
      (is (= 3 (count result)))
      (is (= #{"cat" "car" "card"} (set result)))))

  (testing "Комплексная фильтрация"
    (let [words ["apple" "application" "apply" "banana" "band" "bandana" "can" "candy"]
          ts (apply trie-set words)
          ;; Оставляем только слова длиннее 4 символов
          result (filter #(> (count %) 4) ts)]
      ;; Слова длиннее 4: "apple"(5), "application"(11), "apply"(5), "banana"(6), "bandana"(7), "candy"(5) = 6 слов
      (is (= 6 (count result)))
      (is (= #{"apple" "application" "apply" "banana" "bandana" "candy"} (set result))))))

(deftest trie-set-map-test
  (testing "Map пустого множества"
    (let [ts (trie-set)
          result (map clojure.string/upper-case ts)]
      (is (empty? result))))

  (testing "Добавление префикса к словам"
    (let [ts (trie-set "cat" "dog")
          result (map #(str "prefix-" %) ts)]
      (is (= 2 (count result)))
      (is (= #{"prefix-cat" "prefix-dog"} (set result)))))

  (testing "Добавление суффикса к словам"
    (let [ts (trie-set "test" "word")
          result (map #(str % "-suffix") ts)]
      (is (= 2 (count result)))
      (is (= #{"test-suffix" "word-suffix"} (set result)))))

  (testing "Применение identity функции"
    (let [ts (trie-set "hello" "world")
          result (map identity ts)]
      (is (= 2 (count result)))
      (is (= #{"hello" "world"} (set result)))))

  (testing "Преобразование с пустой строкой в множестве"
    (let [ts (trie-set "" "test")
          result (map #(str % "-mapped") ts)]
      (is (= 2 (count result)))
      (is (= #{"-mapped" "test-mapped"} (set result)))))

  (testing "Исходное множество остается неизменным"
    (let [original (trie-set "original" "words")
          result (map clojure.string/upper-case original)]
      ;; Проверяем, что исходное множество не изменилось
      (is (= 2 (count original)))
      (is (contains? original "original"))
      (is (contains? original "words"))
      (is (not (contains? original "ORIGINAL")))
      (is (not (contains? original "WORDS")))
      
      ;; Проверяем результат
      (is (= 2 (count result)))
      (is (= #{"ORIGINAL" "WORDS"} (set result)))))

  (testing "Замена символов в словах"
    (let [ts (trie-set "hello" "world")
          result (map #(clojure.string/replace % "l" "x") ts)]
      (is (= 2 (count result)))
      (is (= #{"hexxo" "worxd"} (set result)))))

  (testing "Применение сложной функции преобразования"
    (let [ts (trie-set "cat" "dog" "bird")
          ;; Функция: первый символ в верхний регистр + добавить длину
          transform-fn (fn [word]
                        (str (clojure.string/capitalize word) "-" (count word)))
          result (map transform-fn ts)]
      (is (= 3 (count result)))
      (is (= #{"Cat-3" "Dog-3" "Bird-4"} (set result)))))

  (testing "Функция удлиняет слова"
    (let [ts (trie-set "a" "ab")
          result (map #(str % % %) ts)] ; утраивает каждое слово
      (is (= 2 (count result)))
      (is (= #{"aaa" "ababab"} (set result))))))

(deftest trie-set-transduce-test
  (testing "Transduce с простыми операциями"
    (let [ts (trie-set "cat" "dog" "elephant" "ant")]
      ;; Получаем длины слов и суммируем те, что равны 3
      (let [result (transduce
                   (comp (map count) (filter #(= % 3)))
                   +
                   0
                   ts)]
        (is (= 9 result))))) ; 3 слова длиной 3 = 3+3+3=9

  (testing "Into с трансдьюсером"
    (let [ts (trie-set "apple" "banana" "cherry")]
      (let [upper-words (into []
                             (map clojure.string/upper-case)
                             ts)]
        (is (= 3 (count upper-words)))
        (is (every? #(.equals (.toUpperCase %) %) upper-words))))))

(deftest trie-set-standard-functions-test
  (testing "Использование стандартных функций для работы с коллекциями"
    (let [ts (trie-set "apple" "banana" "cherry" "date")]
      
      ;; some - проверка существования элемента с условием
      (is (some #(.startsWith % "a") ts))
      (is (not (some #(.startsWith % "x") ts)))
      
      ;; every? - проверка всех элементов
      (is (every? string? ts))
      (is (not (every? #(> (count %) 5) ts)))
      
      ;; take/drop с seq
      (let [seq-ts (seq ts)]
        (is (= 2 (count (take 2 seq-ts))))
        (is (<= (count (drop 2 seq-ts)) 2)))
      
      ;; group-by
      (let [grouped (group-by first ts)]
        (is (contains? grouped \a))
        (is (contains? grouped \b))
        (is (contains? grouped \c))
        (is (contains? grouped \d)))
      
      ;; sort
      (let [sorted (sort (seq ts))]
        (is (= "apple" (first sorted)))))))

(deftest trie-set-functional-composition-test
  (testing "Композиция функциональных операций"
    (let [ts (trie-set "cat" "car" "card" "dog" "duck" "elephant")]
      
      ;; Фильтрация + map + reduce
      (let [result (->> ts
                       (filter #(.startsWith % "c"))
                       (map count)
                       (reduce +))]
        (is (= 10 result))) ; "cat"(3) + "car"(3) + "card"(4) = 10
      
      ;; Более сложная цепочка
      (let [processed (->> ts
                          (filter #(> (count %) 3))
                          (map clojure.string/upper-case)
                          (filter #(.contains % "A"))
                          set)]
        (is (contains? processed "CARD"))
        (is (contains? processed "ELEPHANT"))
        ;; "DUCK" не содержит "A", поэтому его не должно быть
        (is (not (contains? processed "DUCK")))))))
