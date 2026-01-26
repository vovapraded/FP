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
      (is (= 4 (reduce (fn [acc _word] (inc acc)) 0 ts)))
      (let [words (reduce conj [] ts)]
        (is (= 4 (count words)))
        (is (every? #(contains? ts %) words))
        (is (= #{"cat" "car" "card" "care"} (set words))))))

  (testing "Reduce с суммированием длин слов"
    (let [ts (trie-set "a" "ab" "abc")]
      (is (= 6 (reduce (fn [acc word] (+ acc (count word))) 0 ts)))))

  (testing "Reduce с фильтрацией в процессе"
    (let [ts (trie-set "cat" "car" "dog" "duck")]
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

(deftest filter-set-test
  (testing "Фильтрация пустого множества"
    (let [ts (trie-set)
          result (filter-set (constantly true) ts)]
      (is (= 0 (count result)))
      (is (empty? result))))

  (testing "Фильтрация по предикату"
    (let [ts (trie-set "cat" "car" "dog")
          result (filter-set #(.startsWith % "c") ts)]
      (is (= 2 (count result)))
      (is (contains? result "cat"))
      (is (contains? result "car"))
      (is (not (contains? result "dog")))))

  (testing "Фильтрация по длине слова"
    (let [ts (trie-set "a" "ab" "abc" "abcd")]
      ;; Тест кастомной функции filter-set
      (let [result (filter-set #(>= (count %) 2) ts)]
        (is (= 3 (count result)))
        (is (contains? result "ab"))
        (is (contains? result "abc"))
        (is (contains? result "abcd")))
      ;; Тест стандартной функции filter
      (let [result (filter #(>= (count %) 3) ts)]
        (is (= 2 (count result)))
        (is (= #{"abc" "abcd"} (set result)))))))

(deftest map-set-test
  (testing "Преобразование пустого множества"
    (let [ts (trie-set)
          custom-result (map-set clojure.string/upper-case ts)
          std-result (map clojure.string/upper-case ts)]
      (is (= 0 (count custom-result)))
      (is (empty? custom-result))
      (is (empty? std-result))))

  (testing "Преобразование в верхний регистр"
    (let [ts (trie-set "cat" "dog")]
      ;; Тест кастомной функции map-set
      (let [result (map-set clojure.string/upper-case ts)]
        (is (= 2 (count result)))
        (is (contains? result "CAT"))
        (is (contains? result "DOG")))
      ;; Тест стандартной функции map
      (let [result (map clojure.string/upper-case ts)]
        (is (= 2 (count result)))
        (is (= #{"CAT" "DOG"} (set result))))))

  (testing "Добавление префикса и суффикса"
    (let [ts (trie-set "cat" "dog")]
      ;; Префикс с кастомной функцией
      (let [result (map-set #(str "x-" %) ts)]
        (is (= 2 (count result)))
        (is (contains? result "x-cat"))
        (is (contains? result "x-dog")))
      ;; Суффикс со стандартной функцией
      (let [result (map #(str % "-suffix") ts)]
        (is (= 2 (count result)))
        (is (= #{"cat-suffix" "dog-suffix"} (set result))))))

  (testing "Исходное множество остается неизменным"
    (let [original (trie-set "test")]
      ;; Тест с кастомной функцией
      (let [result (map-set clojure.string/upper-case original)]
        (is (contains? original "test"))
        (is (not (contains? original "TEST")))
        (is (contains? result "TEST")))
      ;; Тест со стандартной функцией
      (let [result (map clojure.string/upper-case original)]
        (is (contains? original "test"))
        (is (= #{"TEST"} (set result))))))

  (testing "Применение сложной функции преобразования"
    (let [ts (trie-set "cat" "dog" "bird")
          transform-fn (fn [word]
                         (str (clojure.string/capitalize word) "-" (count word)))
          result (map transform-fn ts)]
      (is (= 3 (count result)))
      (is (= #{"Cat-3" "Dog-3" "Bird-4"} (set result))))))

(deftest reduce-left-set-test
  (testing "Левая свертка пустого множества"
    (let [ts (trie-set)]
      (is (= 0 (reduce-left-set (fn [acc _] (inc acc)) 0 ts)))
      (is (= [] (reduce-left-set conj [] ts)))))

  (testing "Подсчет элементов левой сверткой"
    (let [ts (trie-set "a" "b" "c")]
      (is (= 3 (reduce-left-set (fn [acc _] (inc acc)) 0 ts)))))

  (testing "Суммирование длин левой сверткой"
    (let [ts (trie-set "a" "ab" "abc")]
      (is (= 6 (reduce-left-set (fn [acc word] (+ acc (count word))) 0 ts)))))

  (testing "Сбор в список левой сверткой"
    (let [ts (trie-set "cat" "dog")]
      (let [result (reduce-left-set conj [] ts)]
        (is (= 2 (count result)))
        (is (= #{"cat" "dog"} (set result)))))))

(deftest reduce-right-set-test
  (testing "Правая свертка пустого множества"
    (let [ts (trie-set)]
      (is (= 0 (reduce-right-set (fn [_ acc] (inc acc)) 0 ts)))))

  (testing "Сравнение левой и правой свертки для коммутативных операций"
    (let [ts (trie-set "a" "b")]
      (let [left-result (reduce-left-set (fn [acc _] (inc acc)) 0 ts)
            right-result (reduce-right-set (fn [_ acc] (inc acc)) 0 ts)]
        (is (= left-result right-result)))))

  (testing "Правая свертка работает корректно"
    (let [ts (trie-set "test")]
      (is (some? (reduce-right-set (fn [_ acc] acc) "default" ts))))))

