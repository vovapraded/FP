(ns lab2.set-operations-test
  (:require [clojure.test :refer :all]
            [lab2.core :refer :all]))

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
    (let [ts (trie-set "a" "ab" "abc")
          result (filter-set #(>= (count %) 2) ts)]
      (is (= 2 (count result)))
      (is (contains? result "ab"))
      (is (contains? result "abc")))))

(deftest map-set-test
  (testing "Преобразование пустого множества"
    (let [ts (trie-set)
          result (map-set clojure.string/upper-case ts)]
      (is (= 0 (count result)))
      (is (empty? result))))

  (testing "Преобразование в верхний регистр"
    (let [ts (trie-set "cat" "dog")
          result (map-set clojure.string/upper-case ts)]
      (is (= 2 (count result)))
      (is (contains? result "CAT"))
      (is (contains? result "DOG"))))

  (testing "Добавление префикса"
    (let [ts (trie-set "cat" "dog")
          result (map-set #(str "x-" %) ts)]
      (is (= 2 (count result)))
      (is (contains? result "x-cat"))
      (is (contains? result "x-dog"))))

  (testing "Исходное множество остается неизменным"
    (let [original (trie-set "test")
          result (map-set clojure.string/upper-case original)]
      (is (contains? original "test"))
      (is (not (contains? original "TEST")))
      (is (contains? result "TEST")))))

(deftest reduce-left-set-test
  (testing "Левая свертка пустого множества"
    (let [ts (trie-set)]
      (is (= 0 (reduce-left-set (fn [acc _] (inc acc)) 0 ts)))
      (is (= [] (reduce-left-set conj [] ts)))))

  (testing "Подсчет элементов"
    (let [ts (trie-set "a" "b" "c")]
      (is (= 3 (reduce-left-set (fn [acc _] (inc acc)) 0 ts)))))

  (testing "Суммирование длин"
    (let [ts (trie-set "a" "ab" "abc")]
      (is (= 6 (reduce-left-set (fn [acc word] (+ acc (count word))) 0 ts)))))

  (testing "Сбор в список"
    (let [ts (trie-set "cat" "dog")]
      (let [result (reduce-left-set conj [] ts)]
        (is (= 2 (count result)))
        (is (= #{"cat" "dog"} (set result)))))))

(deftest reduce-right-set-test
  (testing "Правая свертка пустого множества"
    (let [ts (trie-set)]
      (is (= 0 (reduce-right-set (fn [_ acc] (inc acc)) 0 ts)))))

  (testing "Подсчет элементов с правой сверткой"
    (let [ts (trie-set "a" "b")]
      ;; Простой тест - правая свертка должна давать тот же результат для коммутативных операций
      (let [left-result (reduce-left-set (fn [acc _] (inc acc)) 0 ts)
            right-result (reduce-right-set (fn [_ acc] (inc acc)) 0 ts)]
        (is (= left-result right-result)))))

  (testing "Правая свертка существует и работает"
    (let [ts (trie-set "test")]
      ;; Просто проверяем, что функция работает без ошибок
      (is (some? (reduce-right-set (fn [_ acc] acc) "default" ts))))))
