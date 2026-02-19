(ns lab3.io-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [lab3.io :as io]))

;; ========== PARSE-LINE TESTS ==========

(def ^:private semicolon-regex (re-pattern ";"))
(def ^:private tab-regex (re-pattern "\t"))

(deftest test-parse-line-valid-semicolon
  (testing "Parse valid line with semicolon delimiter"
    (let [result (io/parse-line semicolon-regex "1.5;2.7")]
      (is (= {:x 1.5 :y 2.7} result)))))

(deftest test-parse-line-valid-tab
  (testing "Parse valid line with tab delimiter"
    (let [result (io/parse-line tab-regex "1.5\t2.7")]
      (is (= {:x 1.5 :y 2.7} result)))))

(deftest test-parse-line-with-spaces
  (testing "Parse line with extra spaces"
    (let [result (io/parse-line semicolon-regex "  1.5 ; 2.7  ")]
      (is (= {:x 1.5 :y 2.7} result)))))

(deftest test-parse-line-negative-numbers
  (testing "Parse negative numbers"
    (let [result (io/parse-line semicolon-regex "-1.5;-2.7")]
      (is (= {:x -1.5 :y -2.7} result)))))

(deftest test-parse-line-scientific-notation
  (testing "Parse scientific notation"
    (let [result (io/parse-line semicolon-regex "1.5e2;2.7e-3")]
      (is (= {:x 150.0 :y 0.0027} result)))))

(deftest test-parse-line-empty
  (testing "Empty line returns nil"
    (is (nil? (io/parse-line semicolon-regex "")))))

(deftest test-parse-line-whitespace-only
  (testing "Whitespace-only line returns nil"
    (is (nil? (io/parse-line semicolon-regex "   \t  ")))))

(deftest test-parse-line-invalid-format
  (testing "Invalid format returns nil"
    (is (nil? (io/parse-line semicolon-regex "1.5")))                   ; only one value
    (is (nil? (io/parse-line semicolon-regex "1.5;2.7;3")))             ; three values
    (is (nil? (io/parse-line semicolon-regex "abc;def")))))             ; non-numeric

(deftest test-parse-line-partial-invalid
  (testing "Partial invalid returns nil"
    (is (nil? (io/parse-line semicolon-regex "1.5;abc")))
    (is (nil? (io/parse-line semicolon-regex "abc;2.7")))))

;; ========== PARSE-POINTS TESTS ==========

(deftest test-parse-points-valid
  (testing "Parse multiple valid lines"
    (let [lines ["0;0" "1;1" "2;4"]
          result (io/parse-points ";" lines)]
      (is (= [{:x 0.0 :y 0.0}
              {:x 1.0 :y 1.0}
              {:x 2.0 :y 4.0}]
             (vec result))))))

(deftest test-parse-points-with-invalid
  (testing "Parse lines with some invalid entries"
    (let [lines ["0;0" "invalid" "1;1" "" "2;4"]
          result (io/parse-points ";" lines)]
      (is (= [{:x 0.0 :y 0.0}
              {:x 1.0 :y 1.0}
              {:x 2.0 :y 4.0}]
             (vec result))))))

(deftest test-parse-points-empty
  (testing "Parse empty sequence"
    (let [result (io/parse-points ";" [])]
      (is (empty? result)))))

(deftest test-parse-points-all-invalid
  (testing "Parse all invalid lines"
    (let [lines ["invalid" "also invalid" ""]
          result (io/parse-points ";" lines)]
      (is (empty? result)))))

(deftest test-parse-points-laziness
  (testing "parse-points is lazy"
    (let [lines (cons "0;0" (lazy-seq (throw (Exception. "Should not evaluate"))))
          result (io/parse-points ";" lines)]
      ;; Taking only first element should not throw
      (is (= {:x 0.0 :y 0.0} (first result))))))

;; ========== VALIDATE-SORTED TESTS ==========

(deftest test-validate-sorted-valid
  (testing "Sorted sequence passes validation"
    (let [points [{:x 0 :y 0} {:x 1 :y 1} {:x 2 :y 4}]
          result (io/validate-sorted points)]
      (is (= points (vec result))))))

(deftest test-validate-sorted-equal-x
  (testing "Equal x values throw exception (interpolation requires unique x)"
    (let [points [{:x 0 :y 0} {:x 1 :y 1} {:x 1 :y 2}]]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"Duplicate x coordinate"
                            (vec (io/validate-sorted points)))))))

(deftest test-validate-sorted-invalid
  (testing "Unsorted sequence throws exception"
    (let [points [{:x 0 :y 0} {:x 2 :y 4} {:x 1 :y 1}]]
      (is (thrown? clojure.lang.ExceptionInfo
                   (vec (io/validate-sorted points)))))))

(deftest test-validate-sorted-lazy
  (testing "Validation happens lazily"
    (let [good-points [{:x 0 :y 0} {:x 1 :y 1}]
          bad-point {:x 0.5 :y 0.5}
          points (concat good-points [bad-point])
          result (io/validate-sorted points)]
      ;; Taking first elements should work
      (is (= good-points (vec (take 2 result))))
      ;; Taking third should throw
      (is (thrown? clojure.lang.ExceptionInfo
                   (vec (take 3 result)))))))

(deftest test-validate-sorted-single-point
  (testing "Single point is always sorted"
    (let [points [{:x 5 :y 10}]
          result (io/validate-sorted points)]
      (is (= points (vec result))))))

(deftest test-validate-sorted-empty
  (testing "Empty sequence is sorted"
    (let [result (io/validate-sorted [])]
      (is (empty? result)))))

;; ========== FORMAT-RESULT TESTS ==========

(deftest test-format-result-basic
  (testing "Format basic result"
    (is (= "linear: 1.00 | 2.00" (io/format-result :linear 1.0 2.0)))))

(deftest test-format-result-negative
  (testing "Format negative numbers"
    (is (= "lagrange: -1.00 | -2.00" (io/format-result :lagrange -1.0 -2.0)))))

(deftest test-format-result-precision
  (testing "Format handles precision correctly"
    (let [result (io/format-result :linear 1.23456789 2.98765432)]
      (is (str/includes? result "1.23"))
      (is (str/includes? result "2.99")))))

;; ========== PRINT-RESULT TESTS ==========

(deftest test-print-result
  (testing "Print result produces correct output"
    (let [output (with-out-str (io/print-result! :linear 1.5 2.7))]
      (is (= "linear: 1.50 | 2.70\n" output)))))

(deftest test-print-results
  (testing "Print multiple results"
    (let [results [{:algorithm :linear :x 0.0 :y 0.0}
                   {:algorithm :newton :x 1.0 :y 1.0}]
          output (with-out-str (io/print-results! results))]
      (is (str/includes? output "linear: 0.00 | 0.00"))
      (is (str/includes? output "newton: 1.00 | 1.00")))))
