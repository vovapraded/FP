(ns lab3.stream
  (:require [lab3.interpolation :as interp]))

(defn generate-x-values
  "Никогда не выйдет за end"
  [start end step]
  (->> (/ (- end start) step)
       Math/floor
       long
       inc
       range
       (map #(+ start (* % step)))))

(defn last-x-value
  "Возвращает последнее значение x, которое будет сгенерировано для данного сегмента"
  [start end step]
  (last (generate-x-values start end step)))

(defn create-window [size]
  {:size size
   :points []})

(defn window-full? [{:keys [size points]}]
  (>= (count points) size))

(defn add-to-window
  "Sliding window: если заполнено, удаляется первая точка"
  [{:keys [size points] :as window} point]
  (let [new-points (if (>= (count points) size)
                     (conj (vec (rest points)) point)
                     (conj points point))]
    (assoc window :points new-points)))

(defn get-window-points [{:keys [points]}]
  points)

(defn window-x-range [{:keys [points]}]
  (when (seq points)
    [(:x (first points)) (:x (last points))]))

(defn interpolate-segment
  "Возвращает seq {:algorithm :x :y}"
  [algorithm-key points start-x end-x step]
  (map (fn [x]
         {:algorithm algorithm-key
          :x x
          :y (interp/interpolate algorithm-key points x)})
       (generate-x-values start-x end-x step)))

(defn interpolate-segment-all
  "Интерполирует сегмент для всех алгоритмов сразу.
   Возвращает seq {:algorithm :x :y}"
  [algorithms points start-x end-x step]
  (for [x (generate-x-values start-x end-x step)
        alg algorithms]
    {:algorithm alg
     :x x
     :y (interp/interpolate alg points x)}))

(defn- process-window-segment
  "Возвращает {:start-x :end-x :points}
   
   Логика: интерполируем от последней выведенной точки до максимума текущего окна.
   Для первого окна начинаем с минимума.
   Для не-последнего окна ограничиваем до предпоследней точки, чтобы следующее
   окно могло продолжить интерполяцию с новыми данными."
  [prev-end-x window step is-last]
  (let [[min-x max-x] (window-x-range window)
        points (get-window-points window)
        point-count (count points)]
    (cond
      ;; Недостаточно точек
      (< point-count 2)
      {:start-x nil :end-x nil :points []}

      ;; Первое окно
      (nil? prev-end-x)
      (let [end-x (if is-last
                    max-x
                    ;; Для не-последнего окна останавливаемся на предпоследней точке
                    (:x (nth points (- point-count 2))))]
        {:start-x min-x :end-x end-x :points points})

      ;; Последнее окно - интерполируем до конца
      is-last
      {:start-x (+ prev-end-x step) :end-x max-x :points points}

      ;; Промежуточное окно - интерполируем до предпоследней точки
      :else
      (let [end-x (:x (nth points (- point-count 2)))]
        {:start-x (+ prev-end-x step) :end-x end-x :points points}))))

(defn process-stream [algorithm-key window-size step points output-fn]
  (let [effective-window-size (max window-size interp/min-points)]
    (loop [remaining points
           window (create-window effective-window-size)
           prev-end-x nil]
      (if (seq remaining)
        (let [point (first remaining)
              new-window (add-to-window window point)]
          (if (window-full? new-window)
            (let [{:keys [start-x end-x points]}
                  (process-window-segment prev-end-x new-window step false)
                  results (when (and start-x end-x (seq points) (<= start-x end-x))
                            (interpolate-segment algorithm-key points start-x end-x step))
                  actual-last-x (when (seq results) (last-x-value start-x end-x step))]
              (doseq [r results]
                (output-fn r))
              (recur (rest remaining) new-window (or actual-last-x prev-end-x)))
            (recur (rest remaining) new-window prev-end-x)))
        (when (>= (count (get-window-points window)) 2)
          (let [{:keys [start-x end-x points]}
                (process-window-segment prev-end-x window step true)
                effective-start (if prev-end-x (+ prev-end-x step) start-x)]
            (when (and effective-start end-x (seq points) (<= effective-start end-x))
              (let [results (interpolate-segment algorithm-key points effective-start end-x step)]
                (doseq [r results]
                  (output-fn r))))))))))

(defn initial-state
  "Создаёт начальное состояние процессора с одним общим окном для всех алгоритмов"
  [algorithms window-size]
  (let [effective-window-size (max window-size interp/min-points)]
    {:algorithms algorithms
     :window (create-window effective-window-size)
     :prev-end-x nil}))

(defn step-processor
  "Обрабатывает одну точку. Возвращает {:state :results}"
  [{:keys [algorithms window prev-end-x] :as state} point step]
  (let [new-window (add-to-window window point)]
    (if (window-full? new-window)
      (let [{:keys [start-x end-x points]}
            (process-window-segment prev-end-x new-window step false)
            results (when (and start-x end-x (seq points) (<= start-x end-x))
                      (interpolate-segment-all algorithms points start-x end-x step))
            ;; Сохраняем последнюю фактически выведенную x-координату
            actual-last-x (when (seq results) (last-x-value start-x end-x step))]
        {:state (assoc state :window new-window :prev-end-x (or actual-last-x prev-end-x))
         :results results})
      {:state (assoc state :window new-window)
       :results nil})))

(defn finalize-processor
  "Финализирует обработку, интерполируя оставшиеся точки"
  [{:keys [algorithms window prev-end-x]} step]
  (when (and window (>= (count (get-window-points window)) 2))
    (let [{:keys [start-x end-x points]}
          (process-window-segment prev-end-x window step true)
          effective-start (or (some-> prev-end-x (+ step)) start-x)]
      (when (and effective-start end-x (seq points) (<= effective-start end-x))
        (interpolate-segment-all algorithms points effective-start end-x step)))))

(defn- process-point-reducer
  "Чистая функция-редюсер: аккумулирует состояние и результаты"
  [step {:keys [state accumulated-results]} point]
  (let [{:keys [state results]} (step-processor state point step)]
    {:state state
     :accumulated-results (if results
                            (into accumulated-results results)
                            accumulated-results)}))

(defn process-all-points
  "Обрабатывает все точки и возвращает полную последовательность результатов.
   Чистая функция без побочных эффектов."
  [algorithms window-size step points]
  (let [init-state {:state (initial-state algorithms window-size)
                    :accumulated-results []}
        {:keys [state accumulated-results]}
        (reduce (partial process-point-reducer step) init-state points)
        final-results (finalize-processor state step)]
    (concat accumulated-results final-results)))
