(ns lab3.interpolation)

;; ========== ЗАГЛУШКИ ДЛЯ АЛГОРИТМОВ ИНТЕРПОЛЯЦИИ ==========
;; TODO: Реализовать реальные алгоритмы

(defn linear-interpolate
  "Линейная интерполяция между двумя точками
   points - вектор из двух точек [{:x x1 :y y1} {:x x2 :y y2}]
   x - точка для вычисления
   Возвращает значение y
   Предусловия: points содержит минимум 2 точки с различными x (проверяется в stream/io)"
  [points x]
  ;; y = y1 + (y2 - y1) * (x - x1) / (x2 - x1)
  (let [{x1 :x y1 :y} (first points)
        {x2 :x y2 :y} (second points)]
    (+ y1 (* (- y2 y1) (/ (- x x1) (- x2 x1))))))

(defn newton-interpolate
  "Интерполяция полиномом Ньютона
   points - вектор точек для построения полинома
   x - точка для вычисления
   Возвращает значение y
   Предусловия: points содержит минимум 2 точки с различными x (проверяется в stream/io)"
  [points x]
  ;; TODO: Реализовать интерполяцию Ньютона
  ;; Пока возвращаем линейную интерполяцию как заглушку
  (linear-interpolate (take 2 points) x))

(defn lagrange-interpolate
  "Интерполяция полиномом Лагранжа
   points - вектор точек для построения полинома
   x - точка для вычисления
   Возвращает значение y
   Предусловия: points содержит минимум 2 точки с различными x (проверяется в stream/io)"
  [points x]
  ;; TODO: Реализовать интерполяцию Лагранжа
  ;; Пока возвращаем линейную интерполяцию как заглушку
  (linear-interpolate (take 2 points) x))

;; ========== ДИСПЕТЧЕР АЛГОРИТМОВ ==========

(def algorithms
  "Карта алгоритмов интерполяции"
  {:linear   {:fn linear-interpolate
              :min-points 2}
   :newton   {:fn newton-interpolate
              :min-points 2}
   :lagrange {:fn lagrange-interpolate
              :min-points 2}})

(defn get-interpolation-fn
  "Получить функцию интерполяции по ключу алгоритма"
  [algorithm-key]
  (get-in algorithms [algorithm-key :fn]))

(defn get-min-points
  "Получить минимальное количество точек для алгоритма"
  [algorithm-key]
  (get-in algorithms [algorithm-key :min-points]))

(defn interpolate
  "Выполнить интерполяцию заданным алгоритмом
   algorithm-key - ключ алгоритма (:linear, :newton, :lagrange)
   points - вектор точек
   x - точка для вычисления"
  [algorithm-key points x]
  (if-let [interp-fn (get-interpolation-fn algorithm-key)]
    (interp-fn points x)
    (throw (ex-info "Unknown interpolation algorithm" {:algorithm algorithm-key}))))
