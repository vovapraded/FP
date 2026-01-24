(ns lab2.basic-test
  (:require [clojure.test :refer :all]
            [lab2.core :refer :all]))

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





