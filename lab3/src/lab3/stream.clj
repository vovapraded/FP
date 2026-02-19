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
   Всегда интерполируем до max-x (последней точки окна)."
  [prev-end-x window _step]
  (let [[min-x max-x] (window-x-range window)
        points (get-window-points window)
        point-count (count points)]
    (cond
      ;; Недостаточно точек
      (< point-count 2)
      {:start-x nil :end-x nil :points []}

      ;; Первое окно - интерполируем от min до max
      (nil? prev-end-x)
      {:start-x min-x :end-x max-x :points points}

      ;; Последнее окно или промежуточное - интерполируем до max
      :else
      {:start-x prev-end-x :end-x max-x :points points})))

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
            (process-window-segment prev-end-x new-window step)
            ;; Начало сегмента: для последующих окон начинаем с prev-end-x + step
            segment-start-x (if prev-end-x (+ prev-end-x step) start-x)
            results (when (and segment-start-x end-x (seq points) (<= segment-start-x end-x))
                      (interpolate-segment-all algorithms points segment-start-x end-x step))
            ;; Сохраняем последнюю фактически выведенную x-координату
            actual-last-x (when (seq results) (last-x-value segment-start-x end-x step))]
        {:state (assoc state :window new-window :prev-end-x (or actual-last-x prev-end-x))
         :results results})
      {:state (assoc state :window new-window)
       :results nil})))

(defn finalize-processor
  "Финализирует обработку, интерполируя оставшиеся точки"
  [{:keys [algorithms window prev-end-x]} step]
  (when (and window (>= (count (get-window-points window)) 2))
    (let [{:keys [start-x end-x points]}
          (process-window-segment prev-end-x window step)
          segment-start-x (or (some-> prev-end-x (+ step)) start-x)]
      (when (and segment-start-x end-x (seq points) (<= segment-start-x end-x))
        (interpolate-segment-all algorithms points segment-start-x end-x step)))))
