(ns lab2.trie-set-test
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is testing]]
            [lab2.core :refer [from-seq trie-set trie-set-union]]
            [clojure.string :as str]))

(deftest trie-set-creation-test
  (testing "Создание пустого TrieSet"
    (let [ts (trie-set)]
      (is (= 0 (count ts)))
      (is (empty? ts))
      (is (= #{} (set (seq ts))))))

  (testing "Создание TrieSet с элементами"
    (let [ts (trie-set "cat" "dog" "bird")]
      (is (= 3 (count ts)))
      (is (seq ts))
      (is (= #{"cat" "dog" "bird"} (set (seq ts))))))

  (testing "Создание из последовательности"
    (let [words ["apple" "banana" "cherry"]
          ts (from-seq words)]
      (is (= 3 (count ts)))
      (is (= (set words) (set (seq ts)))))))

(deftest trie-set-basic-operations-test
  (testing "conj операция"
    (let [ts (trie-set)
          ts1 (conj ts "hello")
          ts2 (conj ts1 "world")
          ts3 (conj ts2 "hello")]                           ; дубликат
      (is (= 1 (count ts1)))
      (is (= 2 (count ts2)))
      (is (= 2 (count ts3)))                                ; дубликат не добавился
      (is (contains? ts3 "hello"))
      (is (contains? ts3 "world"))))

  (testing "disj операция"
    (let [ts (trie-set "cat" "dog" "bird")
          ts1 (disj ts "dog")
          ts2 (disj ts1 "missing")]                         ; удаление несуществующего
      (is (= 2 (count ts1)))
      (is (= 2 (count ts2)))                                ; размер не изменился
      (is (not (contains? ts1 "dog")))
      (is (contains? ts1 "cat"))
      (is (contains? ts1 "bird"))))

  (testing "contains? операция"
    (let [ts (trie-set "hello" "world")]
      (is (contains? ts "hello"))
      (is (contains? ts "world"))
      (is (not (contains? ts "missing")))
      (is (not (contains? ts "")))))

  (testing "IFn протокол - вызов как функция"
    (let [ts (trie-set "test" "data")]
      (is (ts "test"))
      (is (ts "data"))
      (is (not (ts "missing"))))))

(deftest trie-set-collection-operations-test
  (testing "empty операция"
    (let [ts (trie-set "a" "b" "c")
          empty-ts (empty ts)]
      (is (= 0 (count empty-ts)))
      (is (empty? empty-ts))
      (is (instance? (class ts) empty-ts))))

  (testing "seq операция"
    (let [ts (trie-set "zebra" "apple" "banana")]
      (let [seq-result (seq ts)]
        (is (not (nil? seq-result)))
        (is (= 3 (count seq-result)))
        (is (= #{"zebra" "apple" "banana"} (set seq-result))))

      ;; Пустое множество должно возвращать nil для seq
      (is (nil? (seq (trie-set))))))

  (testing "count операция"
    (is (= 0 (count (trie-set))))
    (is (= 1 (count (trie-set "single"))))
    (is (= 3 (count (trie-set "a" "b" "c"))))))

(deftest trie-set-equality-test
  (testing "Равенство TrieSet"
    (let [ts1 (trie-set "cat" "dog")
          ts2 (trie-set "dog" "cat")                        ; порядок не важен
          ts3 (trie-set "cat" "bird")
          ts4 (trie-set)]
      (is (.equiv ts1 ts2))
      (is (not (.equiv ts1 ts3)))
      (is (not (.equiv ts1 ts4)))
      (is (.equiv ts4 (trie-set)))))

  (testing "toString представление"
    (let [ts (trie-set "apple" "banana")
          str-repr (.toString ts)]
      (is (.startsWith str-repr "#{"))
      (is (.endsWith str-repr "}"))
      (is (.contains str-repr "apple"))
      (is (.contains str-repr "banana")))))

(deftest trie-set-standard-set-operations-test
  (testing "Совместимость с clojure.set/union"
    (let [ts1 (trie-set "a" "b")
          ts2 (trie-set "b" "c")
          regular-set #{:x :y}]
      ;; union между TrieSet работает через наш метод
      (let [union-result (trie-set-union ts1 ts2)]
        (is (= 3 (count union-result)))
        (is (contains? union-result "a"))
        (is (contains? union-result "b"))
        (is (contains? union-result "c")))

      ;; Проверка что TrieSet ведет себя как set в стандартных операциях
      (let [ts-as-set (set (seq ts1))
            standard-union (set/union ts-as-set regular-set)]
        (is (= 4 (count standard-union))))))

  (testing "Совместимость с filter/map и другими sequence операциями"
    (let [ts (trie-set "apple" "banana" "cherry" "date")]
      ;; filter
      (let [filtered (filter #(.startsWith % "a") ts)]
        (is (= ["apple"] (vec filtered))))

      ;; map
      (let [mapped (map str/upper-case ts)]
        (is (= 4 (count mapped)))
        (is (every? #(.equals (.toUpperCase %) %) mapped)))

      ;; reduce
      (let [total-length (reduce + (map count ts))]
        (is (= (+ 5 6 6 4) total-length)))))

  (testing "Работа с into"
    (let [ts (trie-set "initial")
          extended (into ts ["new1" "new2" "initial"])]     ; initial - дубликат
      (is (= 3 (count extended)))
      (is (contains? extended "initial"))
      (is (contains? extended "new1"))
      (is (contains? extended "new2")))))





(deftest trie-set-reduce-protocols-test
  (testing "IReduceInit протокол - reduce с начальным значением"
    (let [ts (trie-set "apple" "banana" "cherry")]
      ;; Подсчет общей длины всех строк
      (let [total-length (reduce (fn [acc word] (+ acc (count word))) 0 ts)]
        (is (= (+ 5 6 6) total-length)))                    ; apple=5, banana=6, cherry=6

      ;; Сбор всех слов в вектор
      (let [collected (reduce conj [] ts)]
        (is (= 3 (count collected)))
        (is (= (set collected) #{"apple" "banana" "cherry"})))

      ;; Конкатенация строк
      (let [concatenated (reduce str "" ts)]
        (is (string? concatenated))
        (is (= 17 (count concatenated))))))                 ; apple+banana+cherry = 5+6+6=17

  (testing "IReduce протокол - reduce без начального значения"
    (let [ts (trie-set "cat" "dog" "bird")]
      ;; Поиск самого длинного слова
      (let [longest (reduce (fn [acc word]
                              (if (> (count word) (count acc))
                                word
                                acc))
                            ts)]
        (is (= "bird" longest)))                            ; все слова длиной 3-4, bird = 4

      ;; Объединение в одну строку
      (let [combined (reduce str ts)]
        (is (string? combined))
        (is (= 10 (count combined)))))))                    ; cat(3) + dog(3) + bird(4) = 10

