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

(defn- process-window-segment
  "Возвращает {:start-x :end-x :points}"
  [prev-end-x window step is-last]
  (let [[min-x max-x] (window-x-range window)
        points (get-window-points window)
        point-count (count points)]
    (cond
      ;; Недостаточно точек
      (< point-count 2)
      {:start-x nil :end-x nil :points []}

      ;; Первое окно - интерполируем до середины
      (nil? prev-end-x)
      (let [mid-x (if is-last
                    max-x
                    (/ (+ min-x max-x) 2))]
        {:start-x min-x :end-x mid-x :points points})

      ;; Последнее окно - интерполируем от предыдущей точки до конца
      is-last
      {:start-x (+ prev-end-x step) :end-x max-x :points points}

      ;; Промежуточное окно - интерполируем центральную область
      :else
      (let [mid-x (/ (+ min-x max-x) 2)]
        {:start-x (+ prev-end-x step) :end-x mid-x :points points}))))

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
                  (process-window-segment prev-end-x new-window step false)]
              (when (and start-x end-x (seq points))
                (let [results (interpolate-segment algorithm-key points start-x end-x step)]
                  (doseq [r results]
                    (output-fn r))))
              (recur (rest remaining) new-window end-x))
            (recur (rest remaining) new-window prev-end-x)))
        (when (>= (count (get-window-points window)) 2)
          (let [{:keys [start-x end-x points]}
                (process-window-segment prev-end-x window step true)
                effective-start (if prev-end-x (+ prev-end-x step) start-x)]
            (when (and effective-start end-x (seq points) (<= effective-start end-x))
              (let [results (interpolate-segment algorithm-key points effective-start end-x step)]
                (doseq [r results]
                  (output-fn r))))))))))

(defn create-stream-processor [algorithm-key window-size step]
  (fn [points output-fn]
    (process-stream algorithm-key window-size step points output-fn)))

(defn initial-processor-state [algorithm-key window-size]
  (let [effective-window-size (max window-size interp/min-points)]
    {:algorithm algorithm-key
     :window (create-window effective-window-size)
     :prev-end-x nil}))

(defn step-processor
  "Возвращает {:state :results}"
  [{:keys [algorithm window prev-end-x] :as state} point step]
  (let [new-window (add-to-window window point)]
    (if (window-full? new-window)
      (let [{:keys [start-x end-x points]}
            (process-window-segment prev-end-x new-window step false)]
        {:state (assoc state :window new-window :prev-end-x end-x)
         :results (when (and start-x end-x (seq points))
                    (interpolate-segment algorithm points start-x end-x step))})
      {:state (assoc state :window new-window)
       :results nil})))

(defn finalize-processor [{:keys [algorithm window prev-end-x]} step]
  (when (and window (>= (count (get-window-points window)) 2))
    (let [{:keys [start-x end-x points]}
          (process-window-segment prev-end-x window step true)
          effective-start (or (some-> prev-end-x (+ step)) start-x)]
      (when (and effective-start end-x (seq points) (<= effective-start end-x))
        (interpolate-segment algorithm points effective-start end-x step)))))

(defn create-initial-states [algorithms window-size]
  (mapv #(initial-processor-state % window-size) algorithms))

(defn process-point-all
  "Возвращает {:states :all-results}"
  [states point step]
  (reduce
   (fn [{:keys [states all-results]} state]
     (let [{:keys [state results]} (step-processor state point step)]
       {:states (conj states state)
        :all-results (if results
                       (concat all-results results)
                       all-results)}))
   {:states [] :all-results []}
   states))

(defn finalize-all [states step]
  (mapcat #(finalize-processor % step) states))
