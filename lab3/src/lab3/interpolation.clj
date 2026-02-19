(ns lab3.interpolation)

(def min-points 2)

(defn linear-interpolate [points x]
  (let [{x1 :x y1 :y} (first points)
        {x2 :x y2 :y} (second points)]
    (+ y1 (* (- y2 y1) (/ (- x x1) (- x2 x1))))))

(defn- divided-differences
  "Вычисляет таблицу разделённых разностей.
   Возвращает вектор коэффициентов [f[x0], f[x0,x1], f[x0,x1,x2], ...]"
  [points]
  (let [n (count points)
        ys (mapv :y points)
        xs (mapv :x points)]
    (loop [dd ys
           level 1
           coeffs [(first ys)]]
      (if (>= level n)
        coeffs
        (let [new-dd (vec
                      (map-indexed
                       (fn [i _]
                         (/ (- (nth dd (inc i)) (nth dd i))
                            (- (nth xs (+ i level)) (nth xs i))))
                       (range (- n level))))]
          (recur new-dd (inc level) (conj coeffs (first new-dd))))))))

(defn newton-interpolate
  "Интерполяция полиномом Ньютона с разделёнными разностями.
   P(x) = f[x0] + f[x0,x1](x-x0) + f[x0,x1,x2](x-x0)(x-x1) + ..."
  [points x]
  (let [coeffs (divided-differences points)
        xs (mapv :x points)]
    (reduce
     (fn [[sum term] i]
       (let [new-term (* term (- x (nth xs (dec i))))
             coeff (nth coeffs i)]
         [(+ sum (* coeff new-term)) new-term]))
     [(first coeffs) 1.0]
     (range 1 (count coeffs)))
    ;; Результат - первый элемент пары [sum term]
    (first
     (reduce
      (fn [[sum term] i]
        (let [new-term (* term (- x (nth xs (dec i))))
              coeff (nth coeffs i)]
          [(+ sum (* coeff new-term)) new-term]))
      [(first coeffs) 1.0]
      (range 1 (count coeffs))))))

(defn lagrange-interpolate
  "Интерполяция полиномом Лагранжа.
   L(x) = Σ y_i * Π((x - x_j)/(x_i - x_j)) для j ≠ i"
  [points x]
  (let [n (count points)]
    (reduce
     (fn [sum i]
       (let [{xi :x yi :y} (nth points i)
             ;; Вычисляем базисный полином l_i(x)
             li (reduce
                 (fn [prod j]
                   (if (= i j)
                     prod
                     (let [{xj :x} (nth points j)]
                       (* prod (/ (- x xj) (- xi xj))))))
                 1.0
                 (range n))]
         (+ sum (* yi li))))
     0.0
     (range n))))

(def algorithms
  {:linear   linear-interpolate
   :newton   newton-interpolate
   :lagrange lagrange-interpolate})

(defn get-interpolation-fn [algorithm-key]
  (get algorithms algorithm-key))

(defn interpolate [algorithm-key points x]
  (if-let [interp-fn (algorithms algorithm-key)]
    (interp-fn points x)
    (throw (ex-info "Unknown interpolation algorithm" {:algorithm algorithm-key}))))
