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

