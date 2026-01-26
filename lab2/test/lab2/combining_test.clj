(ns lab2.combining-test
  (:require [clojure.test :refer [deftest is testing]]
            [lab2.core :refer [to-trie-node trie-difference trie-intersection trie-set
                               trie-set-union trie-set-union-all trie-to-seq]]))

(deftest trie-set-sequence-test
  (testing "Множество с словами с общими префиксами"
    (let [words ["car" "cat" "card" "care"]
          ts (apply trie-set words)
          result (set (seq ts))]
      (is (= 4 (count result)))
      (is (= (set words) result))))

  (testing "Слово является префиксом другого"
    (let [words ["car" "card"]
          ts (apply trie-set words)
          result (set (seq ts))]
      (is (= 2 (count result)))
      (is (= (set words) result))))

  (testing "После операций удаления"
    (let [original-words ["cat" "car" "card" "care" "dog"]
          ts (apply trie-set original-words)
          modified-ts (-> ts
                          (disj "car")
                          (disj "dog"))
          result (set (seq modified-ts))
          expected-words ["cat" "card" "care"]]
      (is (= 3 (count result)))
      (is (= (set expected-words) result))))

  (testing "Консистентность с размером множества"
    (let [words ["apple" "app" "application" "apply" "banana" "band"]
          ts (apply trie-set words)
          seq-result (seq ts)]
      (is (= (count ts) (count seq-result)))
      (is (= (set words) (set seq-result))))))

(deftest trie-set-union-test
  (testing "Объединение с пустыми множествами"
    (let [empty-ts (trie-set)
          ts (trie-set "cat" "dog")]
      (is (= (set (seq ts)) (set (seq (trie-set-union empty-ts ts)))))
      (is (= (set (seq ts)) (set (seq (trie-set-union ts empty-ts)))))
      (is (empty? (trie-set-union empty-ts empty-ts)))))

  (testing "Пересекающиеся слова - критический случай"
    (let [ts1 (trie-set "car" "card" "care")
          ts2 (trie-set "car" "cat" "call")
          union (trie-set-union ts1 ts2)]
      (is (= 5 (count union)))
      (is (= #{"car" "card" "care" "cat" "call"} (set (seq union))))
      ;; Проверяем структурную целостность
      (is (contains? union "car"))
      (is (not (contains? union "ca")))))

  (testing "Непересекающиеся множества"
    (let [ts1 (trie-set "apple" "banana")
          ts2 (trie-set "cherry" "date")
          union (trie-set-union ts1 ts2)]
      (is (= 4 (count union)))
      (is (= #{"apple" "banana" "cherry" "date"} (set (seq union))))
      ;; Проверяем, что все слова присутствуют
      (is (contains? union "apple"))
      (is (contains? union "banana"))
      (is (contains? union "cherry"))
      (is (contains? union "date"))
      ;; Проверяем, что нет лишних слов
      (is (not (contains? union "grape")))
      (is (not (contains? union "app")))))

  (testing "Множественные объединения"
    (let [ts1 (trie-set "a" "b")
          ts2 (trie-set "c" "d")
          ts3 (trie-set "e" "f")
          result (trie-set-union-all ts1 ts2 ts3)]
      (is (= 6 (count result)))
      (is (every? #(contains? result %) ["a" "b" "c" "d" "e" "f"])))))

(deftest trie-set-equality-and-equiv-test
  (testing "Равенство множеств"
    (let [ts1 (trie-set "cat" "dog")
          ts2 (trie-set "dog" "cat")                        ; порядок не важен
          ts3 (trie-set "cat" "bird")]
      (is (.equiv ts1 ts2))
      (is (not (.equiv ts1 ts3)))))

  (testing "Пустые множества равны"
    (let [empty1 (trie-set)
          empty2 (trie-set)]
      (is (.equiv empty1 empty2))))

  (testing "toString представление"
    (let [ts (trie-set "apple" "banana")
          str-repr (.toString ts)]
      (is (.startsWith str-repr "#{"))
      (is (.endsWith str-repr "}"))
      (is (.contains str-repr "apple"))
      (is (.contains str-repr "banana")))))

(deftest trie-intersection-comprehensive-test
  (testing "Базовые случаи пересечения"
    (let [ts1 (to-trie-node (trie-set "cat" "car" "card"))
          ts2 (to-trie-node (trie-set "cat" "dog" "card"))
          result (trie-intersection ts1 ts2)]
      (is (= 2 (:node-count result)))
      (is (= #{"cat" "card"} (set (trie-to-seq result))))))

  (testing "Пересечение с пустым множеством"
    (let [ts (to-trie-node (trie-set "cat" "dog"))
          empty (to-trie-node (trie-set))
          result1 (trie-intersection ts empty)
          result2 (trie-intersection empty ts)]
      (is (= 0 (:node-count result1)))
      (is (= 0 (:node-count result2)))
      (is (empty? (trie-to-seq result1)))
      (is (empty? (trie-to-seq result2)))))

  (testing "Пересечение непересекающихся множеств"
    (let [ts1 (to-trie-node (trie-set "cat" "car"))
          ts2 (to-trie-node (trie-set "dog" "bird"))
          result (trie-intersection ts1 ts2)]
      (is (= 0 (:node-count result)))
      (is (empty? (trie-to-seq result)))))

  (testing "Пересечение идентичных множеств"
    (let [ts (to-trie-node (trie-set "cat" "car" "card"))
          result (trie-intersection ts ts)]
      (is (= 3 (:node-count result)))
      (is (= #{"cat" "car" "card"} (set (trie-to-seq result))))))

  (testing "Пересечение с общими префиксами"
    (let [ts1 (to-trie-node (trie-set "cat" "car" "card" "care"))
          ts2 (to-trie-node (trie-set "ca" "car" "care" "call"))
          result (trie-intersection ts1 ts2)]
      (is (= 2 (:node-count result)))
      (is (= #{"car" "care"} (set (trie-to-seq result))))))

  (testing "Пересечение подмножеств"
    (let [ts1 (to-trie-node (trie-set "cat" "car"))
          ts2 (to-trie-node (trie-set "cat" "car" "card" "care"))
          result (trie-intersection ts1 ts2)]
      (is (= 2 (:node-count result)))
      (is (= #{"cat" "car"} (set (trie-to-seq result))))))

  (testing "Оптимизация счетчиков при пересечении"
    (let [ts1 (to-trie-node (trie-set "a" "ab" "abc"))
          ts2 (to-trie-node (trie-set "a" "ax" "axy"))
          result (trie-intersection ts1 ts2)]
      ;; Должен содержать только "a"
      (is (= 1 (:node-count result)))
      (is (= #{"a"} (set (trie-to-seq result))))
      ;; Проверим правильность счетчиков внутренних узлов
      (is (= 1 (:node-count result)))
      (is (= 1 (:node-count (get (:children result) \a)))))))

(deftest trie-difference-comprehensive-test
  (testing "Базовые случаи разности"
    (let [ts1 (to-trie-node (trie-set "cat" "car" "card"))
          ts2 (to-trie-node (trie-set "cat" "dog"))
          result (trie-difference ts1 ts2)]
      (is (= 2 (:node-count result)))
      (is (= #{"car" "card"} (set (trie-to-seq result))))))

  (testing "Разность с пустым множеством"
    (let [ts (to-trie-node (trie-set "cat" "dog"))
          empty (to-trie-node (trie-set))
          result1 (trie-difference ts empty)
          result2 (trie-difference empty ts)]
      ;; ts - empty = ts
      (is (= 2 (:node-count result1)))
      (is (= #{"cat" "dog"} (set (trie-to-seq result1))))
      ;; empty - ts = empty
      (is (= 0 (:node-count result2)))
      (is (empty? (trie-to-seq result2)))))

  (testing "Разность непересекающихся множеств"
    (let [ts1 (to-trie-node (trie-set "cat" "car"))
          ts2 (to-trie-node (trie-set "dog" "bird"))
          result (trie-difference ts1 ts2)]
      ;; Должно остаться все из ts1
      (is (= 2 (:node-count result)))
      (is (= #{"cat" "car"} (set (trie-to-seq result))))))

  (testing "Разность идентичных множеств"
    (let [ts (to-trie-node (trie-set "cat" "car" "card"))
          result (trie-difference ts ts)]
      (is (= 0 (:node-count result)))
      (is (empty? (trie-to-seq result)))))

  (testing "Разность с общими префиксами"
    (let [ts1 (to-trie-node (trie-set "cat" "car" "card" "care"))
          ts2 (to-trie-node (trie-set "ca" "car" "call"))
          result (trie-difference ts1 ts2)]
      ;; Должно остаться: "cat", "card", "care"
      (is (= 3 (:node-count result)))
      (is (= #{"cat" "card" "care"} (set (trie-to-seq result))))))

  (testing "Разность подмножества из надмножества"
    (let [ts1 (to-trie-node (trie-set "cat" "car" "card" "care"))
          ts2 (to-trie-node (trie-set "cat" "car"))
          result (trie-difference ts1 ts2)]
      (is (= 2 (:node-count result)))
      (is (= #{"card" "care"} (set (trie-to-seq result)))))))
