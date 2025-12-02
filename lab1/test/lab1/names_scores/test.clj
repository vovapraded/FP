(ns lab1.names-scores.test
  (:require [clojure.test :refer [are deftest]]
            [lab1.names-scores.tail-recursive :as tail-recursive]
            [lab1.names-scores.recursive :as recursive]
            [lab1.names-scores.modular :as modular]
            [lab1.names-scores.loop :as loop]))

(def impls
  [tail-recursive/total-score
   recursive/total-score
   modular/total-score
   loop/total-score])

(deftest basic-cases
  (are [names expected] (every? #(= expected (% names)) impls)
    ["A"] 1
    ["B"] 2
    ["Z"] 26
    ["A" "B"] 5
    ["B" "A"] 5
    ["AB"] 3
    ["BA"] 3
    ["AA"] 2
    ["ABC"] 6))

(deftest edge-cases
  (are [names expected] (every? #(= expected (% names)) impls)
    [] 0
    [""] 0
    ["" ""] 0
    ["A" ""] 2
    ["" "A"] 2))

(deftest sorting-test
  (are [names expected] (every? #(= expected (% names)) impls)
    ["C" "A" "B"] 14
    ["Z" "A"] 53
    ["B" "A" "C"] 14
    ["COLIN"] 53
    ["COLIN" "AARON"] 155))

(deftest multi-letter-names
  (are [names expected] (every? #(= expected (% names)) impls)
    ["AA" "AB"] 8
    ["ABC" "BAC"] 18
    ["XYZ"] 75
    ["A" "AA" "AAA"] 14))

(deftest identical-names
  (are [names expected] (every? #(= expected (% names)) impls)
    ["A" "A"] 3
    ["A" "A" "A"] 6
    ["AB" "AB"] 9
    ["ABC" "ABC" "ABC"] 36))

(deftest longer-names
  (are [names expected] (every? #(= expected (% names)) impls)
    ["ABCDEFGHIJ"] 55
    ["ABCDEFGHIJKLMNOPQRSTUVWXYZ"] 351
    ["MARY" "PATRICIA" "LINDA"] 385))

(deftest example-euler-case
  (let [test-names ["COLIN" "AARON" "MARY"]]
    (are [names expected] (every? #(= expected (% names)) impls)
      test-names 326)))
