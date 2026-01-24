(ns lab2.core-test
  (:require [clojure.test :refer :all]
            [lab2.core :refer :all]))

(deftest basic-trie-test
  (testing "Основные операции с trie"
    (let [trie (trie-set "cat" "car" "dog")]
      (is (= 3 (trie-size trie)))
      (is (trie-contains? trie "cat"))
      (is (trie-contains? trie "car"))
      (is (trie-contains? trie "dog"))
      (is (not (trie-contains? trie "bird"))))))

(deftest trie-insert-comprehensive-test
  (testing "Вставка слов с общим префиксом"
    (let [trie (-> empty-node
                   (trie-insert "car")
                   (trie-insert "cat")
                   (trie-insert "card"))]
      (is (= 3 (trie-size trie)))
      (is (trie-contains? trie "car"))
      (is (trie-contains? trie "cat"))
      (is (trie-contains? trie "card"))
      (is (not (trie-contains? trie "ca")))))

  (testing "Вставка префикса существующего слова"
    (let [trie (trie-insert empty-node "card")
          result (trie-insert trie "car")]
      (is (= 2 (trie-size result)))
      (is (trie-contains? result "car"))
      (is (trie-contains? result "card"))))

  (testing "Вставка пустой строки"
    (let [result (trie-insert empty-node "")]
      (is (= 1 (trie-size result)))
      (is (trie-contains? result ""))
      (is (:terminal? result)))))

(deftest trie-remove-comprehensive-test
  (testing "Удаление слова с общим префиксом"
    (let [trie (-> empty-node
                   (trie-insert "car")
                   (trie-insert "cat")
                   (trie-insert "card"))
          result (trie-remove trie "car")]
      (is (= 2 (trie-size result)))
      (is (not (trie-contains? result "car")))
      (is (trie-contains? result "cat"))
      (is (trie-contains? result "card"))))

  (testing "Удаление префикса другого слова"
    (let [trie (-> empty-node
                   (trie-insert "car")
                   (trie-insert "card"))
          result (trie-remove trie "car")]
      (is (= 1 (trie-size result)))
      (is (not (trie-contains? result "car")))
      (is (trie-contains? result "card"))))

  (testing "Очистка узлов после удаления"
    (let [trie (-> empty-node
                   (trie-insert "abc")
                   (trie-insert "ab"))
          result (trie-remove trie "abc")]
      (is (= 1 (trie-size result)))
      (is (trie-contains? result "ab"))
      (is (not (trie-contains? result "abc")))
      ;; Проверяем, что узел 'c' был удален
      (is (nil? (get-in result [:children \a :children \b :children \c]))))))

(deftest trie-insert-remove-integration-test
  (testing "Последовательность операций вставки и удаления"
    (let [trie (-> empty-node
                   (trie-insert "hello")
                   (trie-insert "world")
                   (trie-remove "hello")
                   (trie-insert "hell")
                   (trie-insert "hello"))]
      (is (= 3 (trie-size trie)))
      (is (trie-contains? trie "world"))
      (is (trie-contains? trie "hell"))
      (is (trie-contains? trie "hello"))
      (is (not (trie-empty? trie)))))

  (testing "Удаление всех слов должно дать пустое дерево"
    (let [trie (-> empty-node
                   (trie-insert "cat")
                   (trie-insert "dog")
                   (trie-insert "bird"))
          result (-> trie
                     (trie-remove "cat")
                     (trie-remove "dog")
                     (trie-remove "bird"))]
      (is (= 0 (trie-size result)))
      (is (trie-empty? result))
      (is (not (trie-contains? result "cat")))
      (is (not (trie-contains? result "dog")))
      (is (not (trie-contains? result "bird"))))))

(deftest trie-edge-cases-test
  (testing "Работа со специальными символами"
    (let [words ["hello!" "@test" "#hash" "символ"]
          trie (reduce trie-insert empty-node words)]
      (is (= 4 (trie-size trie)))
      (doseq [word words]
        (is (trie-contains? trie word)))
      (let [result (reduce trie-remove trie words)]
        (is (= 0 (trie-size result)))
        (is (trie-empty? result)))))

  (testing "Сложные префиксные отношения"
    (let [words ["a" "aa" "aaa" "aaaa" "aaaab"]
          trie (reduce trie-insert empty-node words)]
      (is (= 5 (trie-size trie)))
      (doseq [word words]
        (is (trie-contains? trie word)))
      ;; Удаляем средний элемент
      (let [result (trie-remove trie "aaa")]
        (is (= 4 (trie-size result)))
        (is (not (trie-contains? result "aaa")))
        (is (trie-contains? result "a"))
        (is (trie-contains? result "aa"))
        (is (trie-contains? result "aaaa"))
        (is (trie-contains? result "aaaab"))))))

(deftest trie-to-seq-comprehensive-test
  (testing "Дерево с словами с общими префиксами"
    (let [words ["car" "cat" "card" "care"]
          trie (reduce trie-insert empty-node words)
          result (set (trie-to-seq trie))]
      (is (= 4 (count result)))
      (is (= (set words) result))))

  (testing "Слово является префиксом другого"
    (let [words ["car" "card"]
          trie (reduce trie-insert empty-node words)
          result (set (trie-to-seq trie))]
      (is (= 2 (count result)))
      (is (= (set words) result))))

  (testing "После операций удаления"
    (let [original-words ["cat" "car" "card" "care" "dog"]
          trie (reduce trie-insert empty-node original-words)
          modified-trie (-> trie
                           (trie-remove "car")
                           (trie-remove "dog"))
          result (set (trie-to-seq modified-trie))
          expected-words ["cat" "card" "care"]]
      (is (= 3 (count result)))
      (is (= (set expected-words) result))))

  (testing "Консистентность с размером дерева"
    (let [words ["apple" "app" "application" "apply" "banana" "band"]
          trie (reduce trie-insert empty-node words)
          seq-result (trie-to-seq trie)]
      (is (= (trie-size trie) (count seq-result)))
      (is (= (set words) (set seq-result))))))

(deftest trie-union-test
  (testing "Объединение с пустыми деревьями"
    (let [empty (make-node)
          trie (trie-set "cat" "dog")]
      (is (= (trie-to-seq trie) (trie-to-seq (trie-union empty trie))))
      (is (= (trie-to-seq trie) (trie-to-seq (trie-union trie empty))))
      (is (trie-empty? (trie-union empty empty)))))

  (testing "Пересекающиеся слова - критический случай"
    (let [trie1 (trie-set "car" "card" "care")
          trie2 (trie-set "car" "cat" "call")
          union (trie-union trie1 trie2)]
      (is (= 5 (trie-size union)))
      (is (= #{"car" "card" "care" "cat" "call"} (set (trie-to-seq union))))
      ;; Проверяем структурную целостность
      (is (trie-contains? union "car"))
      (is (not (trie-contains? union "ca"))))))