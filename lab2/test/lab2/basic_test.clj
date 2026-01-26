(ns lab2.basic-test
  (:require [clojure.test :refer :all]
            [lab2.core :refer :all]))

(deftest trie-set-insert-comprehensive-test
  (testing "Добавление слов с общим префиксом"
    (let [ts (-> (trie-set)
                 (conj "car")
                 (conj "cat")
                 (conj "card"))]
      (is (= 3 (count ts)))
      (is (contains? ts "car"))
      (is (contains? ts "cat"))
      (is (contains? ts "card"))
      (is (not (contains? ts "ca")))))

  (testing "Добавление префикса существующего слова"
    (let [ts (conj (trie-set) "card")
          result (conj ts "car")]
      (is (= 2 (count result)))
      (is (contains? result "car"))
      (is (contains? result "card"))))

  (testing "Добавление пустой строки"
    (let [result (conj (trie-set) "")]
      (is (= 1 (count result)))
      (is (contains? result ""))))

  (testing "Дубликаты не добавляются"
    (let [ts (trie-set "hello" "world")
          result (conj ts "hello")]
      (is (= 2 (count result)))
      (is (contains? result "hello"))
      (is (contains? result "world")))))

(deftest trie-set-remove-comprehensive-test
  (testing "Удаление слова с общим префиксом"
    (let [ts (trie-set "car" "cat" "card")
          result (disj ts "car")]
      (is (= 2 (count result)))
      (is (not (contains? result "car")))
      (is (contains? result "cat"))
      (is (contains? result "card"))))

  (testing "Удаление префикса другого слова"
    (let [ts (trie-set "car" "card")
          result (disj ts "car")]
      (is (= 1 (count result)))
      (is (not (contains? result "car")))
      (is (contains? result "card"))))

  (testing "Удаление несуществующего элемента"
    (let [ts (trie-set "hello" "world")
          result (disj ts "missing")]
      (is (= 2 (count result)))
      (is (contains? result "hello"))
      (is (contains? result "world"))))

  (testing "Структурная очистка после удаления"
    (let [ts (trie-set "abc" "ab")
          result (disj ts "abc")]
      (is (= 1 (count result)))
      (is (contains? result "ab"))
      (is (not (contains? result "abc"))))))

(deftest trie-set-basic-operations-test
  (testing "Пустое множество"
    (let [ts (trie-set)]
      (is (= 0 (count ts)))
      (is (empty? ts))
      (is (nil? (seq ts)))))

  (testing "Создание с элементами"
    (let [ts (trie-set "apple" "banana" "cherry")]
      (is (= 3 (count ts)))
      (is (not (empty? ts)))
      (is (= #{"apple" "banana" "cherry"} (set (seq ts))))))

  (testing "Вызов как функция"
    (let [ts (trie-set "test" "data")]
      (is (ts "test"))
      (is (ts "data"))
      (is (not (ts "missing"))))))

