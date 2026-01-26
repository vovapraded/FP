(ns lab2.trie-set-functions-test
  (:require [clojure.test :refer :all]
            [lab2.core :refer :all]))

(deftest trie-set-functional-operations-test
  (testing "filter-set возвращает TrieSet"
    (let [ts (trie-set "apple" "banana" "cherry" "apricot")
          filtered (filter-set #(.startsWith % "a") ts)]
      ;; Проверяем, что результат - это TrieSet
      (is (instance? lab2.trie_set.TrieSet filtered))
      (is (= 2 (count filtered)))
      (is (contains? filtered "apple"))
      (is (contains? filtered "apricot"))
      (is (not (contains? filtered "banana")))
      (is (not (contains? filtered "cherry")))
      
      ;; Можем использовать как функцию
      (is (filtered "apple"))
      (is (not (filtered "banana")))))

  (testing "map-set возвращает TrieSet"
    (let [ts (trie-set "cat" "dog" "bird")
          mapped (map-set clojure.string/upper-case ts)]
      ;; Проверяем, что результат - это TrieSet
      (is (instance? lab2.trie_set.TrieSet mapped))
      (is (= 3 (count mapped)))
      (is (contains? mapped "CAT"))
      (is (contains? mapped "DOG"))
      (is (contains? mapped "BIRD"))
      (is (not (contains? mapped "cat")))
      
      ;; Можем использовать как функцию
      (is (mapped "CAT"))
      (is (not (mapped "cat")))))

  (testing "fold-set использует эффективную реализацию"
    (let [ts (trie-set "hello" "world" "test")]
      ;; Подсчитываем общее количество символов
      (let [total-chars (fold-set (fn [acc word] (+ acc (count word))) 0 ts)]
        (is (= 14 total-chars))) ; hello(5) + world(5) + test(4) = 14
      
      ;; Собираем все слова в список
      (let [words (fold-set conj [] ts)]
        (is (= 3 (count words)))
        (is (= (set words) #{"hello" "world" "test"})))))

  (testing "Цепочка функциональных операций"
    (let [ts (trie-set "apple" "application" "banana" "app" "cherry")
          result (->> ts
                     (filter-set #(.startsWith % "app"))  ; оставляем только app*
                     (map-set #(str % "-processed"))     ; добавляем суффикс
                     (filter-set #(> (count %) 15)))]    ; оставляем длинные
      ;; Должно остаться только "application-processed"
      (is (instance? lab2.trie_set.TrieSet result))
      (is (= 1 (count result)))
      (is (contains? result "application-processed"))))

  (testing "Сравнение с обычными filter/map"
    (let [ts (trie-set "cat" "car" "card" "dog")]
      ;; Обычные filter/map возвращают последовательности
      (let [std-filtered (filter #(.startsWith % "ca") ts)
            std-mapped (map clojure.string/upper-case ts)]
        (is (not (instance? lab2.trie_set.TrieSet std-filtered)))
        (is (not (instance? lab2.trie_set.TrieSet std-mapped)))
        (is (= 3 (count std-filtered)))
        (is (= 4 (count std-mapped))))
      
      ;; Наши функции возвращают TrieSet
      (let [our-filtered (filter-set #(.startsWith % "ca") ts)
            our-mapped (map-set clojure.string/upper-case ts)]
        (is (instance? lab2.trie_set.TrieSet our-filtered))
        (is (instance? lab2.trie_set.TrieSet our-mapped))
        (is (= 3 (count our-filtered)))
        (is (= 4 (count our-mapped)))))))