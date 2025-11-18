(ns lab1.problem8.test
  (:require [clojure.test :refer [are deftest]]
            [lab1.problem8.s1-1 :refer [max-product-tail-recursion]]
            [lab1.problem8.s1-2 :refer [max-product-recursion]]
            [lab1.problem8.s2 :refer [max-product-modular]]
            [lab1.problem8.s3 :refer [max-product-map-generation]]
            [lab1.problem8.s4 :refer [max-product-loop]]
            [lab1.problem8.s5 :refer [max-product-modular-for-lazy-collections]]))

(defn string-to-digits [s]
  (mapv #(- (int %) (int \0)) s))

(def impls
  [max-product-tail-recursion
   max-product-recursion
   max-product-modular
   max-product-map-generation
   max-product-loop
   max-product-modular-for-lazy-collections])

(deftest basic-cases
  (are [digits window expected] (every? #(= expected (% digits window)) impls)
    [1 2 3 4] 2 12
    [9 8 7 6] 3 504
    [1 0 2 9] 2 18
    [3 3 3 3] 3 27
    [5 5 5] 2 25
    [1 2] 2 2
    [2 3 4] 3 24))

(deftest edge-cases
  (are [digits window expected] (every? #(= expected (% digits window)) impls)
    [] 1 ##-Inf
    [] 3 ##-Inf
    [5] 1 5
    [5] 2 ##-Inf
    [3 4] 1 4
    [3 4] 2 12
    [3 4] 3 ##-Inf
    [1 2 3 4] 0 ##-Inf
    [1 2 3 4] -1 ##-Inf
    [1 2 3 4] 10 ##-Inf))

(deftest zeros-test
  (are [digits window expected] (every? #(= expected (% digits window)) impls)
    [0] 1 0
    [0 0 0] 2 0
    [1 0 5 6] 2 30
    [0 1 2 3] 3 6
    [1 2 3 0] 3 6))

(deftest max-digits
  (are [digits window expected] (every? #(= expected (% digits window)) impls)
    [9 9 9] 2 81
    [9 9 9 9] 4 6561
    [1 9 1 9] 2 9
    [0 9 0 9] 2 0))

(deftest sequences-test
  (are [digits window expected] (every? #(= expected (% digits window)) impls)
    [9 8 7 6 5] 3 504
    [1 2 3 4 5] 3 60
    [1 2 3 4 5 6 7 8 9] 1 9
    [1 2 3 4 5 6 7 8 9] 9 362880))

(def euler-digits
  (delay (string-to-digits
          (str "7316717653133062491922511967442657474235534919493496983520312774"
               "5063262395783180169848018694788518438586156078911294949545950173"
               "7958331952853208805511125406987471585238630507156932909632952274"
               "4304355766896648950445244523161731856403098711121722383113622298"
               "9342338030813533627661428280644448664523874930358907296290491560"
               "4407723907138105158593079608667017242712188399879790879227492190"
               "1699720888093776657273330010533678812202354218097512545405947522"
               "4352584907711670556013604839586446706324415722155397536978179778"
               "4617406495514929086256932197846862248283972241375657056057490261"
               "4079729686524145351004748216637048440319989000889524345065854122"
               "7588666881164271714799244429282308634656748139191231628245861786"
               "6458359124566529476545682848912883142607690042242190226710556263"
               "2111110937054421750694165896040807198403850962455444362981230987"
               "8799272442849091888458015616609791913387549920052406368991256071"
               "7606058861164671094050775410022569831552000559357297257163626956"
               "1882670428252483600823257530420752963450"))))

(deftest euler-test
  (let [digits @euler-digits]
    (are [window expected] (every? #(= expected (% digits window)) impls)
      1 9
      2 81
      3 648
      4 5832
      13 23514624000)))

(deftest real-strings
  (are [s window expected] (every? #(= expected (% (string-to-digits s) window)) impls)
    "123456789" 3 504
    "987654321" 4 3024
    "1111111" 5 1
    "1020304" 2 0
    "999888777" 3 729))
