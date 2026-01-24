(ns lab2.functional-test
  (:require [clojure.test :refer :all]
            [lab2.core :refer :all]
            [lab2.functional :as func]))

(deftest trie-fold-test
  (testing "Свертка пустого trie"
    (is (= 0 (func/trie-fold + 0 empty-node)))
    (is (= [] (func/trie-fold conj [] empty-node))))

  (testing "Свертка с одним словом"
    (let [trie (trie-insert empty-node "cat")]
      (is (= 1 (func/trie-fold (fn [acc _word] (inc acc)) 0 trie)))
      (is (= ["cat"] (func/trie-fold conj [] trie)))))

  (testing "Свертка с несколькими словами"
    (let [trie (-> empty-node
                   (trie-insert "cat")
                   (trie-insert "car")
                   (trie-insert "card")
                   (trie-insert "care"))]
      ;; Подсчет количества слов
      (is (= 4 (func/trie-fold (fn [acc _word] (inc acc)) 0 trie)))
      
      ;; Сбор всех слов в список
      (let [words (func/trie-fold conj [] trie)]
        (is (= 4 (count words)))
        (is (every? #(trie-contains? trie %) words))
        (is (contains? (set words) "cat"))
        (is (contains? (set words) "car"))
        (is (contains? (set words) "card"))
        (is (contains? (set words) "care")))))

  (testing "Свертка с суммированием длин слов"
    (let [trie (-> empty-node
                   (trie-insert "a")
                   (trie-insert "ab")
                   (trie-insert "abc"))]
      (is (= 6 (func/trie-fold (fn [acc word] (+ acc (count word))) 0 trie)))))

  (testing "Свертка с конкатенацией строк"
    (let [trie (-> empty-node
                   (trie-insert "hello")
                   (trie-insert "world"))]
      (let [result (func/trie-fold str "" trie)]
        (is (or (= "helloworld" result)
                (= "worldhello" result))))))

  (testing "Свертка с фильтрацией в процессе"
    (let [trie (-> empty-node
                   (trie-insert "cat")
                   (trie-insert "car")
                   (trie-insert "dog")
                   (trie-insert "duck"))]
      ;; Считаем только слова, начинающиеся с 'c'
      (is (= 2 (func/trie-fold (fn [acc word]
                                 (if (.startsWith word "c")
                                   (inc acc)
                                   acc))
                               0 trie)))))

  (testing "Свертка с пустой строкой в trie"
    (let [trie (-> empty-node
                   (trie-insert "")
                   (trie-insert "cat"))]
      (is (= 2 (func/trie-fold (fn [acc _word] (inc acc)) 0 trie)))
      (let [words (set (func/trie-fold conj [] trie))]
        (is (contains? words ""))
        (is (contains? words "cat"))))))

(deftest trie-filter-test
  (testing "Фильтрация пустого trie"
    (let [result (func/trie-filter (constantly true) empty-node)]
      (is (trie-empty? result))))

  (testing "Фильтрация с предикатом, который всегда возвращает true"
    (let [trie (-> empty-node
                   (trie-insert "cat")
                   (trie-insert "car")
                   (trie-insert "dog"))
          result (func/trie-filter (constantly true) trie)]
      (is (= 3 (trie-size result)))
      (is (trie-contains? result "cat"))
      (is (trie-contains? result "car"))
      (is (trie-contains? result "dog"))))

  (testing "Фильтрация с предикатом, который всегда возвращает false"
    (let [trie (-> empty-node
                   (trie-insert "cat")
                   (trie-insert "car")
                   (trie-insert "dog"))
          result (func/trie-filter (constantly false) trie)]
      (is (trie-empty? result))))

  (testing "Фильтрация по длине слова"
    (let [trie (-> empty-node
                   (trie-insert "a")
                   (trie-insert "ab")
                   (trie-insert "abc")
                   (trie-insert "abcd"))
          result (func/trie-filter #(>= (count %) 3) trie)]
      (is (= 2 (trie-size result)))
      (is (trie-contains? result "abc"))
      (is (trie-contains? result "abcd"))
      (is (not (trie-contains? result "a")))
      (is (not (trie-contains? result "ab")))))

  (testing "Фильтрация по началу слова"
    (let [trie (-> empty-node
                   (trie-insert "cat")
                   (trie-insert "car")
                   (trie-insert "card")
                   (trie-insert "dog")
                   (trie-insert "duck"))
          result (func/trie-filter #(.startsWith % "ca") trie)]
      (is (= 3 (trie-size result)))
      (is (trie-contains? result "cat"))
      (is (trie-contains? result "car"))
      (is (trie-contains? result "card"))
      (is (not (trie-contains? result "dog")))
      (is (not (trie-contains? result "duck")))))


  (testing "Фильтрация trie"
    (let [words ["apple" "application" "apply" "banana" "band" "bandana" "can" "candy"]
          trie (reduce trie-insert empty-node words)
          ;; Оставляем только слова длиннее 4 символов
          result (func/trie-filter #(> (count %) 4) trie)]
      ;; Слова длиннее 4: "apple"(5), "application"(11), "apply"(5), "banana"(6), "bandana"(7), "candy"(5) = 6 слов
      (is (= 6 (trie-size result)))
      (is (trie-contains? result "apple"))
      (is (trie-contains? result "application"))
      (is (trie-contains? result "apply"))
      (is (trie-contains? result "banana"))
      (is (trie-contains? result "bandana"))
      (is (trie-contains? result "candy"))
      (is (not (trie-contains? result "band"))) ; длина 4
      (is (not (trie-contains? result "can")))))) ; длина 3

(deftest trie-map-test
  (testing "Применение функции к пустому trie"
    (let [result (func/trie-map clojure.string/upper-case empty-node)]
      (is (trie-empty? result))))


  (testing "Добавление префикса к словам"
    (let [trie (-> empty-node
                   (trie-insert "cat")
                   (trie-insert "dog"))
          result (func/trie-map #(str "prefix-" %) trie)]
      (is (= 2 (trie-size result)))
      (is (trie-contains? result "prefix-cat"))
      (is (trie-contains? result "prefix-dog"))
      (is (not (trie-contains? result "cat")))
      (is (not (trie-contains? result "dog")))))

  (testing "Добавление суффикса к словам"
    (let [trie (-> empty-node
                   (trie-insert "test")
                   (trie-insert "word"))
          result (func/trie-map #(str % "-suffix") trie)]
      (is (= 2 (trie-size result)))
      (is (trie-contains? result "test-suffix"))
      (is (trie-contains? result "word-suffix"))))

  (testing "Применение identity функции"
    (let [trie (-> empty-node
                   (trie-insert "hello")
                   (trie-insert "world"))
          result (func/trie-map identity trie)]
      (is (= 2 (trie-size result)))
      (is (trie-contains? result "hello"))
      (is (trie-contains? result "world"))))


  (testing "Преобразование с пустой строкой в trie"
    (let [trie (-> empty-node
                   (trie-insert "")
                   (trie-insert "test"))
          result (func/trie-map #(str % "-mapped") trie)]
      (is (= 2 (trie-size result)))
      (is (trie-contains? result "-mapped")) ; пустая строка + суффикс
      (is (trie-contains? result "test-mapped"))))

  (testing "Исходный trie остается неизменным"
    (let [original (-> empty-node
                       (trie-insert "original")
                       (trie-insert "words"))
          result (func/trie-map clojure.string/upper-case original)]
      ;; Проверяем, что исходный trie не изменился
      (is (= 2 (trie-size original)))
      (is (trie-contains? original "original"))
      (is (trie-contains? original "words"))
      (is (not (trie-contains? original "ORIGINAL")))
      (is (not (trie-contains? original "WORDS")))
      
      ;; Проверяем результат
      (is (= 2 (trie-size result)))
      (is (trie-contains? result "ORIGINAL"))
      (is (trie-contains? result "WORDS"))))

  (testing "Замена символов в словах"
    (let [trie (-> empty-node
                   (trie-insert "hello")
                   (trie-insert "world"))
          result (func/trie-map #(clojure.string/replace % "l" "x") trie)]
      (is (= 2 (trie-size result)))
      (is (trie-contains? result "hexxo"))
      (is (trie-contains? result "worxd"))))

  (testing "Применение сложной функции преобразования"
    (let [trie (-> empty-node
                   (trie-insert "cat")
                   (trie-insert "dog")
                   (trie-insert "bird"))
          ;; Функция: первый символ в верхний регистр + добавить длину
          transform-fn (fn [word]
                         (str (clojure.string/capitalize word) "-" (count word)))
          result (func/trie-map transform-fn trie)]
      (is (= 3 (trie-size result)))
      (is (trie-contains? result "Cat-3"))
      (is (trie-contains? result "Dog-3"))
      (is (trie-contains? result "Bird-4"))))


  (testing "Функция удлиняет слова"
    (let [trie (-> empty-node
                   (trie-insert "a")
                   (trie-insert "ab"))
          result (func/trie-map #(str % % %) trie)] ; утраивает каждое слово
      (is (= 2 (trie-size result)))
      (is (trie-contains? result "aaa"))
      (is (trie-contains? result "ababab")))))

