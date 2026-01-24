(ns lab2.combining-test
  (:require [clojure.test :refer :all]
            [lab2.core :refer :all]))

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
      (is (not (trie-contains? union "ca")))))

  (testing "Непересекающиеся множества"
    (let [trie1 (trie-set "apple" "banana")
          trie2 (trie-set "cherry" "date")
          union (trie-union trie1 trie2)]
      (is (= 4 (trie-size union)))
      (is (= #{"apple" "banana" "cherry" "date"} (set (trie-to-seq union))))
      ;; Проверяем, что все слова присутствуют
      (is (trie-contains? union "apple"))
      (is (trie-contains? union "banana"))
      (is (trie-contains? union "cherry"))
      (is (trie-contains? union "date"))
      ;; Проверяем, что нет лишних слов
      (is (not (trie-contains? union "grape")))
      (is (not (trie-contains? union "app"))))))