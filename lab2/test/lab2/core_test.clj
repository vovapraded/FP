(ns lab2.core-test
  (:require [clojure.test :refer :all]
            [lab2.core :refer :all]))

(deftest trie-set-test
  (testing "Создание пустого Trie-множества"
    (let [empty-trie (trie-set)]
      (is (= false (trie-contains? empty-trie "anything")))))
  
  (testing "Добавление и проверка одного слова"
    (let [trie (trie-set "cat")]
      (is (= true (trie-contains? trie "cat")))
      (is (= false (trie-contains? trie "ca")))
      (is (= false (trie-contains? trie "cats")))))
  
  (testing "Добавление нескольких слов"
    (let [trie (trie-set "cat" "car" "cart" "dog")]
      (is (= true (trie-contains? trie "cat")))
      (is (= true (trie-contains? trie "car"))) 
      (is (= true (trie-contains? trie "cart")))
      (is (= true (trie-contains? trie "dog")))
      (is (= false (trie-contains? trie "ca")))
      (is (= false (trie-contains? trie "camel")))
      (is (= false (trie-contains? trie "do"))))))

(deftest insert-test
  (testing "Поэтапное добавление слов"
    (let [trie (-> empty-node
                   (insert "hello")
                   (insert "world")
                   (insert "help"))]
      (is (= true (trie-contains? trie "hello")))
      (is (= true (trie-contains? trie "world")))
      (is (= true (trie-contains? trie "help")))
      (is (= false (trie-contains? trie "hel")))
      (is (= false (trie-contains? trie "helping"))))))

(deftest edge-cases-test
  (testing "Пустые строки и особые случаи"
    (let [trie (trie-set "" "a")]
      (is (= true (trie-contains? trie "")))
      (is (= true (trie-contains? trie "a")))
      (is (= false (trie-contains? trie "aa"))))))
