(ns lab3.interpolation-test
  (:require [clojure.test :refer [deftest is testing]]
            [lab3.interpolation :as interp]))

(def epsilon 1e-9)

(defn approx=
  "Проверка приближённого равенства с точностью epsilon"
  [expected actual]
  (< (Math/abs (- expected actual)) epsilon))

;; === Тестовые данные ===

(def linear-points
  "Точки на прямой y = 2x + 1"
  [{:x 0 :y 1} {:x 1 :y 3} {:x 2 :y 5}])

(def quadratic-points
  "Точки на параболе y = x²"
  [{:x 0 :y 0} {:x 1 :y 1} {:x 2 :y 4}])

(def cubic-points
  "Точки на кубической функции y = x³"
  [{:x -1 :y -1} {:x 0 :y 0} {:x 1 :y 1} {:x 2 :y 8}])

;; === Свойство: интерполяция в узлах возвращает значения узлов ===

(deftest polynomial-at-nodes-test
  (testing "Полиномиальные методы возвращают точные значения в узлах"
    (doseq [alg [:lagrange :newton]
            points [linear-points quadratic-points cubic-points]
            {:keys [x y]} points]
      (is (approx= y (interp/interpolate alg points x))
          (format "%s at node x=%s should return y=%s" alg x y)))))

(deftest linear-at-last-two-nodes-test
  (testing "Линейная интерполяция точна в последних двух узлах"
    (doseq [points [linear-points quadratic-points cubic-points]]
      (let [n (count points)
            {x1 :x y1 :y} (nth points (- n 2))
            {x2 :x y2 :y} (nth points (- n 1))]
        (is (approx= y1 (interp/linear-interpolate points x1)))
        (is (approx= y2 (interp/linear-interpolate points x2)))))))

;; === Свойство: линейная интерполяция точна для линейных функций ===

(deftest linear-exact-for-linear-function-test
  (testing "Линейная интерполяция точна для линейной функции y = 2x + 1"
    (doseq [x [0.5 1.5 -0.5 0.25 1.75]]
      (let [expected (+ 1 (* 2 x))
            actual (interp/linear-interpolate linear-points x)]
        (is (approx= expected actual)
            (format "linear at x=%s: expected %s, got %s" x expected actual))))))

;; === Свойство: полиномиальные методы точны для соответствующих степеней ===

(deftest lagrange-exact-for-quadratic-test
  (testing "Лагранж точен для квадратичной функции y = x²"
    (doseq [x [0.5 1.5 -0.5 0.25 1.75]]
      (let [expected (* x x)
            actual (interp/lagrange-interpolate quadratic-points x)]
        (is (approx= expected actual)
            (format "lagrange at x=%s: expected %s, got %s" x expected actual))))))

(deftest newton-exact-for-quadratic-test
  (testing "Ньютон точен для квадратичной функции y = x²"
    (doseq [x [0.5 1.5 -0.5 0.25 1.75]]
      (let [expected (* x x)
            actual (interp/newton-interpolate quadratic-points x)]
        (is (approx= expected actual)
            (format "newton at x=%s: expected %s, got %s" x expected actual))))))

;; === Свойство: Lagrange и Newton дают одинаковые результаты ===

(deftest lagrange-newton-equivalence-test
  (testing "Лагранж и Ньютон дают одинаковые результаты"
    (doseq [points [quadratic-points cubic-points]
            x [0.5 1.5 -0.5 0.25 1.75]]
      (let [lagr (interp/lagrange-interpolate points x)
            newt (interp/newton-interpolate points x)]
        (is (approx= lagr newt)
            (format "lagrange=%s newton=%s at x=%s" lagr newt x))))))

;; === Свойство: кубическая интерполяция точна для кубической функции ===

(deftest cubic-exact-test
  (testing "4 точки достаточно для точной интерполяции y = x³"
    (doseq [x [0.5 1.5 -0.5 0.25 1.75]]
      (let [expected (* x x x)
            lagr (interp/lagrange-interpolate cubic-points x)
            newt (interp/newton-interpolate cubic-points x)]
        (is (approx= expected lagr)
            (format "lagrange cubic at x=%s: expected %s, got %s" x expected lagr))
        (is (approx= expected newt)
            (format "newton cubic at x=%s: expected %s, got %s" x expected newt))))))

;; === Граничный случай: две точки ===

(deftest two-points-test
  (testing "Все методы работают с двумя точками"
    (let [pts [{:x 0 :y 0} {:x 2 :y 4}]]
      (is (approx= 2.0 (interp/linear-interpolate pts 1)))
      (is (approx= 2.0 (interp/lagrange-interpolate pts 1)))
      (is (approx= 2.0 (interp/newton-interpolate pts 1))))))

;; === Тест неизвестного алгоритма ===

(deftest unknown-algorithm-test
  (testing "Неизвестный алгоритм выбрасывает исключение"
    (is (thrown? clojure.lang.ExceptionInfo
                 (interp/interpolate :unknown quadratic-points 0.5)))))
