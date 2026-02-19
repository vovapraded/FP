(ns lab3.stream
  (:require [lab3.interpolation :as interp]))

;; ========== ГЕНЕРАЦИЯ X-КООРДИНАТ ==========

(defn generate-x-values
  "Генерирует последовательность X-координат с заданным шагом
   от start до end (включительно, с небольшой погрешностью)"
  [start end step]
  (take-while #(<= % (+ end 0.0001))
              (iterate #(+ % step) start)))

;; ========== УПРАВЛЕНИЕ ОКНОМ ДАННЫХ ==========

(defn create-window
  "Создать новое окно данных"
  [size]
  {:size size
   :points []})

(defn window-full?
  "Проверить, заполнено ли окно"
  [{:keys [size points]}]
  (>= (count points) size))

(defn add-to-window
  "Добавить точку в окно (sliding window)
   Если окно заполнено, удаляется первая точка"
  [{:keys [size points] :as window} point]
  (let [new-points (if (>= (count points) size)
                     (conj (vec (rest points)) point)
                     (conj points point))]
    (assoc window :points new-points)))

(defn get-window-points
  "Получить точки из окна"
  [{:keys [points]}]
  points)

(defn window-x-range
  "Получить диапазон X-координат окна [min-x max-x]"
  [{:keys [points]}]
  (when (seq points)
    [(:x (first points)) (:x (last points))]))

;; ========== ПОТОКОВАЯ ОБРАБОТКА ==========

(defn interpolate-segment
  "Интерполировать сегмент между точками окна
   algorithm-key - ключ алгоритма
   points - точки для интерполяции
   start-x, end-x - диапазон X
   step - шаг дискретизации
   Возвращает последовательность {:algorithm :x :y}"
  [algorithm-key points start-x end-x step]
  (let [x-values (generate-x-values start-x end-x step)]
    (map (fn [x]
           {:algorithm algorithm-key
            :x x
            :y (interp/interpolate algorithm-key points x)})
         x-values)))

(defn- process-window-segment
  "Обработать сегмент окна и определить диапазон для интерполяции
   prev-end-x - предыдущая конечная X (или nil для первого окна)
   window - текущее окно
   step - шаг дискретизации
   is-last - последнее ли это окно"
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

(defn process-stream
  "Обработать поток точек с потоковой интерполяцией
   algorithm-key - ключ алгоритма
   window-size - размер окна
   step - шаг дискретизации
   points - ленивая последовательность точек
   output-fn - функция вывода (принимает результат интерполяции)
   
   Возвращает nil, выводит результаты через output-fn"
  [algorithm-key window-size step points output-fn]
  (let [min-points (interp/get-min-points algorithm-key)
        effective-window-size (max window-size min-points)]
    (loop [remaining points
           window (create-window effective-window-size)
           prev-end-x nil
           first-output? true]
      (if (seq remaining)
        ;; Есть ещё точки - добавляем в окно
        (let [point (first remaining)
              new-window (add-to-window window point)]
          (if (window-full? new-window)
            ;; Окно заполнено - интерполируем
            (let [{:keys [start-x end-x points]} 
                  (process-window-segment prev-end-x new-window step false)]
              (when (and start-x end-x (seq points))
                (let [results (interpolate-segment algorithm-key points start-x end-x step)]
                  (doseq [r results]
                    (output-fn r))))
              (recur (rest remaining) new-window end-x false))
            ;; Окно не заполнено - продолжаем накапливать
            (recur (rest remaining) new-window prev-end-x first-output?)))
        ;; Точки закончились - обрабатываем последнее окно
        (when (>= (count (get-window-points window)) 2)
          (let [{:keys [start-x end-x points]}
                (process-window-segment prev-end-x window step true)
                effective-start (if prev-end-x (+ prev-end-x step) start-x)]
            (when (and effective-start end-x (seq points) (<= effective-start end-x))
              (let [results (interpolate-segment algorithm-key points effective-start end-x step)]
                (doseq [r results]
                  (output-fn r))))))))))

;; ========== ЛИНЕЙНАЯ ИНТЕРПОЛЯЦИЯ (ОСОБЫЙ СЛУЧАЙ) ==========

(defn process-linear-stream
  "Специальная обработка для линейной интерполяции
   Интерполирует между каждой парой соседних точек
   algorithm-key - ключ алгоритма (обычно :linear)
   step - шаг дискретизации
   points - последовательность точек
   output-fn - функция вывода"
  [algorithm-key step points output-fn]
  (loop [remaining points
         prev-point nil
         prev-end-x nil]
    (if (seq remaining)
      (let [point (first remaining)]
        (if prev-point
          ;; Есть предыдущая точка - интерполируем между ними
          (let [start-x (if prev-end-x 
                          (+ prev-end-x step)
                          (:x prev-point))
                end-x (:x point)
                two-points [prev-point point]]
            (when (<= start-x end-x)
              (let [results (interpolate-segment algorithm-key two-points start-x end-x step)]
                (doseq [r results]
                  (output-fn r))))
            (recur (rest remaining) point end-x))
          ;; Первая точка - просто сохраняем
          (recur (rest remaining) point nil)))
      ;; Если была только одна точка, выводим её
      (when (and prev-point (nil? prev-end-x))
        (output-fn {:algorithm algorithm-key :x (:x prev-point) :y (:y prev-point)})))))

;; ========== ВЫСОКОУРОВНЕВЫЙ API ==========

(defn create-stream-processor
  "Создать обработчик потока для заданного алгоритма
   Возвращает функцию (fn [points output-fn] ...)"
  [algorithm-key window-size step]
  (if (= algorithm-key :linear)
    ;; Линейная интерполяция использует особую обработку
    (fn [points output-fn]
      (process-linear-stream algorithm-key step points output-fn))
    ;; Остальные алгоритмы используют общую обработку с окном
    (fn [points output-fn]
      (process-stream algorithm-key window-size step points output-fn))))

;; ========== ЧИСТО ФУНКЦИОНАЛЬНАЯ ОБРАБОТКА ПОТОКА ==========

(defn initial-processor-state
  "Создать начальное состояние процессора для алгоритма
   Чистая функция - никакого мутабельного состояния"
  [algorithm-key window-size]
  (let [min-points (interp/get-min-points algorithm-key)
        effective-window-size (max window-size min-points)]
    {:algorithm algorithm-key
     :window (create-window effective-window-size)
     :prev-end-x nil}))

(defn initial-linear-state
  "Создать начальное состояние для линейной интерполяции"
  [algorithm-key]
  {:algorithm algorithm-key
   :prev-point nil
   :prev-end-x nil})

(defn step-processor
  "Обработать одну точку процессором (чистая функция)
   Возвращает {:state новое-состояние :results seq-результатов}"
  [state point step]
  (let [{:keys [algorithm window prev-end-x]} state
        new-window (add-to-window window point)]
    (if (window-full? new-window)
      ;; Окно заполнено - интерполируем
      (let [{:keys [start-x end-x points]} 
            (process-window-segment prev-end-x new-window step false)
            results (when (and start-x end-x (seq points))
                      (interpolate-segment algorithm points start-x end-x step))]
        {:state (assoc state :window new-window :prev-end-x end-x)
         :results results})
      ;; Окно не заполнено
      {:state (assoc state :window new-window)
       :results nil})))

(defn step-linear-processor
  "Обработать одну точку для линейной интерполяции (чистая функция)"
  [state point step]
  (let [{:keys [algorithm prev-point prev-end-x]} state]
    (if prev-point
      ;; Есть предыдущая точка - интерполируем
      (let [start-x (if prev-end-x 
                      (+ prev-end-x step)
                      (:x prev-point))
            end-x (:x point)
            two-points [prev-point point]
            results (when (<= start-x end-x)
                      (interpolate-segment algorithm two-points start-x end-x step))]
        {:state (assoc state :prev-point point :prev-end-x end-x)
         :results results})
      ;; Первая точка
      {:state (assoc state :prev-point point)
       :results nil})))

(defn finalize-processor
  "Финализировать процессор (чистая функция)
   Возвращает финальные результаты"
  [state step]
  (let [{:keys [algorithm window prev-end-x]} state]
    (when (and window (>= (count (get-window-points window)) 2))
      (let [{:keys [start-x end-x points]}
            (process-window-segment prev-end-x window step true)
            effective-start (if prev-end-x (+ prev-end-x step) start-x)]
        (when (and effective-start end-x (seq points) (<= effective-start end-x))
          (interpolate-segment algorithm points effective-start end-x step))))))

(defn finalize-linear-processor
  "Финализировать линейный процессор"
  [state]
  (let [{:keys [algorithm prev-point prev-end-x]} state]
    (when (and prev-point (nil? prev-end-x))
      [{:algorithm algorithm :x (:x prev-point) :y (:y prev-point)}])))

(defn create-initial-states
  "Создать начальные состояния для всех алгоритмов"
  [algorithms window-size]
  (mapv (fn [alg]
          (if (= alg :linear)
            (initial-linear-state alg)
            (initial-processor-state alg window-size)))
        algorithms))

(defn process-point-all
  "Обработать одну точку всеми процессорами
   states - вектор состояний
   point - точка
   step - шаг
   Возвращает {:states новые-состояния :all-results seq-всех-результатов}"
  [states point step]
  (reduce
    (fn [{:keys [states all-results]} state]
      (let [{:keys [algorithm]} state
            {:keys [state results]} (if (= algorithm :linear)
                                      (step-linear-processor state point step)
                                      (step-processor state point step))]
        {:states (conj states state)
         :all-results (if results 
                        (concat all-results results)
                        all-results)}))
    {:states [] :all-results []}
    states))

(defn finalize-all
  "Финализировать все процессоры
   Возвращает seq всех финальных результатов"
  [states step]
  (mapcat
    (fn [state]
      (let [{:keys [algorithm]} state]
        (if (= algorithm :linear)
          (finalize-linear-processor state)
          (finalize-processor state step))))
    states))
