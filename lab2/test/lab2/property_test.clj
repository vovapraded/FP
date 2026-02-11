(ns lab2.property-test
  (:require [clojure.test :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [lab2.core :as core]))

;; Генераторы для тестов
(def gen-string
  "Генератор строк для TrieSet"
  (gen/one-of [gen/string-alphanumeric
               (gen/elements ["cat" "car" "card" "care" "dog" "bird" "apple" "app"])]))

(def gen-trie-set
  "Генератор TrieSet"
  (gen/fmap #(apply core/trie-set %) (gen/vector gen-string 0 10)))

(def gen-three-trie-sets
  "Генератор трех TrieSet для тестирования ассоциативности"
  (gen/tuple gen-trie-set gen-trie-set gen-trie-set))

;; Property-based тесты для моноидальных свойств

(defspec monoid-associativity-property 100
  (prop/for-all [[a b c] gen-three-trie-sets]
                (let [left-assoc (core/trie-set-union (core/trie-set-union a b) c)
                      right-assoc (core/trie-set-union a (core/trie-set-union b c))]
                  (.equiv left-assoc right-assoc))))

(defspec monoid-identity-property-left 100
  (prop/for-all [a gen-trie-set]
                (let [empty-set (core/trie-set)
                      result (core/trie-set-union empty-set a)]
                  (.equiv result a))))

(defspec monoid-identity-property-right 100
  (prop/for-all [a gen-trie-set]
                (let [empty-set (core/trie-set)
                      result (core/trie-set-union a empty-set)]
                  (.equiv result a))))

(defspec idempotence-property 100
  (prop/for-all [a gen-trie-set]
                (let [result (core/trie-set-union a a)]
                  (.equiv result a))))

(defspec commutativity-property 100
  (prop/for-all [a gen-trie-set
                 b gen-trie-set]
                (let [ab (core/trie-set-union a b)
                      ba (core/trie-set-union b a)]
                  (.equiv ab ba))))

(defspec union-size-property 100
  (prop/for-all [a gen-trie-set
                 b gen-trie-set]
                (let [union-set (core/trie-set-union a b)
                      union-size (count union-set)
                      sum-sizes (+ (count a) (count b))]
                  (<= union-size sum-sizes))))

(defspec union-contains-all-property 100
  (prop/for-all [a gen-trie-set
                 b gen-trie-set]
                (let [union-set (core/trie-set-union a b)]
                  (and
                   (every? #(contains? union-set %) (seq a))
                   (every? #(contains? union-set %) (seq b))))))

;; Дополнительные свойства для полноты тестирования

(defspec insertion-consistency-property 100
  (prop/for-all [trie-set gen-trie-set
                 element gen-string]
                (let [original-size (count trie-set)
                      new-set (conj trie-set element)
                      new-size (count new-set)
                      size-diff (- new-size original-size)]
                  (and (<= 0 size-diff 1)
                       (contains? new-set element)))))

(defspec removal-consistency-property 100
  (prop/for-all [trie-set gen-trie-set
                 element gen-string]
                (let [original-size (count trie-set)
                      new-set (disj trie-set element)
                      new-size (count new-set)
                      size-diff (- original-size new-size)]
                  (and (<= 0 size-diff 1)
                       (not (contains? new-set element))))))

(defspec seq-consistency-property 100
  (prop/for-all [trie-set gen-trie-set]
                (let [seq-count (count (seq trie-set))
                      trie-count (count trie-set)]
                  (= seq-count trie-count))))

