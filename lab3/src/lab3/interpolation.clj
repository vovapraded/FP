(ns lab3.interpolation)

;; ========== КОНСТАНТЫ ==========

(def min-points
  "Минимальное количество точек для любой интерполяции"
  2)

;; ========== АЛГОРИТМЫ ИНТЕРПОЛЯЦИИ ==========

(defn linear-interpolate
  "Линейная интерполяция между двумя точками
   points - вектор из двух точек [{:x x1 :y y1} {:x x2 :y y2}]
   x - точка для вычисления
   Возвращает значение y"
  [points x]
  (let [{x1 :x y1 :y} (first points)
        {x2 :x y2 :y} (second points)]
    (+ y1 (* (- y2 y1) (/ (- x x1) (- x2 x1))))))

(defn newton-interpolate
  "Интерполяция полиномом Ньютона
   points - вектор точек для построения полинома
   x - точка для вычисления
   Возвращает значение y"
  [points x]
  ;; TODO: Реализовать интерполяцию Ньютона
  (linear-interpolate (take 2 points) x))

(defn lagrange-interpolate
  "Интерполяция полиномом Лагранжа
   points - вектор точек для построения полинома
   x - точка для вычисления
   Возвращает значение y"
  [points x]
  ;; TODO: Реализовать интерполяцию Лагранжа
  (linear-interpolate (take 2 points) x))

;; ========== ДИСПЕТЧЕР АЛГОРИТМОВ ==========

(def algorithms
  "Карта алгоритмов интерполяции"
  {:linear   linear-interpolate
   :newton   newton-interpolate
   :lagrange lagrange-interpolate})

(defn get-interpolation-fn
  "Получить функцию интерполяции по ключу алгоритма"
  [algorithm-key]
  (get algorithms algorithm-key))

(defn interpolate
  "Выполнить интерполяцию заданным алгоритмом
   algorithm-key - ключ алгоритма (:linear, :newton, :lagrange)
   points - вектор точек
   x - точка для вычисления"
  [algorithm-key points x]
  (if-let [interp-fn (get-interpolation-fn algorithm-key)]
    (interp-fn points x)
    (throw (ex-info "Unknown interpolation algorithm" {:algorithm algorithm-key}))))
