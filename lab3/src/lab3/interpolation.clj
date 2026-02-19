(ns lab3.interpolation)

(def min-points 2)

(defn linear-interpolate [points x]
  (let [{x1 :x y1 :y} (first points)
        {x2 :x y2 :y} (second points)]
    (+ y1 (* (- y2 y1) (/ (- x x1) (- x2 x1))))))

(defn newton-interpolate [points x]
  ;; TODO: Реализовать интерполяцию Ньютона
  (linear-interpolate (take 2 points) x))

(defn lagrange-interpolate [points x]
  ;; TODO: Реализовать интерполяцию Лагранжа
  (linear-interpolate (take 2 points) x))

(def algorithms
  {:linear   linear-interpolate
   :newton   newton-interpolate
   :lagrange lagrange-interpolate})

(defn get-interpolation-fn [algorithm-key]
  (get algorithms algorithm-key))

(defn interpolate [algorithm-key points x]
  (if-let [interp-fn (get-interpolation-fn algorithm-key)]
    (interp-fn points x)
    (throw (ex-info "Unknown interpolation algorithm" {:algorithm algorithm-key}))))
