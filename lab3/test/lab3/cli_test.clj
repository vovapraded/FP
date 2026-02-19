(ns lab3.cli-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [lab3.cli :as cli]))

(deftest test-parse-cli-args-with-linear
  (testing "Parse linear interpolation flag"
    (let [result (cli/parse-cli-args ["--linear"])]
      (is (:ok? result))
      (is (get-in result [:options :linear])))))

(deftest test-parse-cli-args-with-multiple-algorithms
  (testing "Parse multiple algorithms"
    (let [result (cli/parse-cli-args ["--linear" "--newton"])]
      (is (:ok? result))
      (is (get-in result [:options :linear]))
      (is (get-in result [:options :newton])))))

(deftest test-parse-cli-args-with-step
  (testing "Parse step parameter"
    (let [result (cli/parse-cli-args ["--linear" "--step" "0.5"])]
      (is (:ok? result))
      (is (= 0.5 (get-in result [:options :step]))))))

(deftest test-parse-cli-args-with-window-size
  (testing "Parse window size parameter"
    (let [result (cli/parse-cli-args ["--newton" "-w" "5"])]
      (is (:ok? result))
      (is (= 5 (get-in result [:options :window-size]))))))

(deftest test-parse-cli-args-with-delimiter
  (testing "Parse custom delimiter"
    (let [result (cli/parse-cli-args ["--linear" "-d" "\t"])]
      (is (:ok? result))
      (is (= "\t" (get-in result [:options :delimiter]))))))

(deftest test-parse-cli-args-default-values
  (testing "Default values are set"
    (let [result (cli/parse-cli-args ["--linear"])]
      (is (:ok? result))
      (is (= 1.0 (get-in result [:options :step])))
      (is (= 4 (get-in result [:options :window-size])))
      (is (= ";" (get-in result [:options :delimiter]))))))

(deftest test-parse-cli-args-no-algorithm
  (testing "Error when no algorithm specified"
    (let [result (cli/parse-cli-args ["--step" "0.5"])]
      (is (not (:ok? result)))
      (is (string? (:exit-message result)))
      (is (re-find #"at least one" (str/lower-case (:exit-message result)))))))

(deftest test-parse-cli-args-invalid-step
  (testing "Error with negative step"
    (let [result (cli/parse-cli-args ["--linear" "--step" "-1"])]
      (is (not (:ok? result)))
      (is (string? (:exit-message result))))))

(deftest test-parse-cli-args-invalid-step-zero
  (testing "Error with zero step"
    (let [result (cli/parse-cli-args ["--linear" "--step" "0"])]
      (is (not (:ok? result))))))

(deftest test-parse-cli-args-invalid-window-size
  (testing "Error with window size < 2"
    (let [result (cli/parse-cli-args ["--newton" "-w" "1"])]
      (is (not (:ok? result))))))

(deftest test-parse-cli-args-invalid-number-format
  (testing "Error with non-numeric step"
    (let [result (cli/parse-cli-args ["--linear" "--step" "abc"])]
      (is (not (:ok? result)))
      (is (string? (:exit-message result))))))

(deftest test-parse-cli-args-help
  (testing "Help flag returns usage"
    (let [result (cli/parse-cli-args ["--help"])]
      (is (:ok? result))
      (is (string? (:exit-message result)))
      (is (re-find #"Usage" (:exit-message result))))))

(deftest test-get-selected-algorithms-single
  (testing "Get single selected algorithm"
    (let [options {:linear true :newton false :lagrange false}
          algorithms (cli/get-selected-algorithms options)]
      (is (= [:linear] algorithms)))))

(deftest test-get-selected-algorithms-multiple
  (testing "Get multiple selected algorithms"
    (let [options {:linear true :newton true :lagrange false}
          algorithms (cli/get-selected-algorithms options)]
      (is (= 2 (count algorithms)))
      (is (some #{:linear} algorithms))
      (is (some #{:newton} algorithms)))))

(deftest test-get-selected-algorithms-none
  (testing "Get empty vector when no algorithms selected"
    (let [options {:linear false :newton false :lagrange false}
          algorithms (cli/get-selected-algorithms options)]
      (is (empty? algorithms)))))

(deftest test-get-selected-algorithms-all
  (testing "Get all algorithms when all selected"
    (let [options {:linear true :newton true :lagrange true}
          algorithms (cli/get-selected-algorithms options)]
      (is (= 3 (count algorithms)))
      (is (some #{:linear} algorithms))
      (is (some #{:newton} algorithms))
      (is (some #{:lagrange} algorithms)))))

(deftest test-short-options
  (testing "Short option flags work"
    (let [result (cli/parse-cli-args ["-l" "-n" "-s" "0.7" "-w" "3"])]
      (is (:ok? result))
      (is (get-in result [:options :linear]))
      (is (get-in result [:options :newton]))
      (is (= 0.7 (get-in result [:options :step])))
      (is (= 3 (get-in result [:options :window-size]))))))
