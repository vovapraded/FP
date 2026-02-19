(ns lab3.core
  (:require [lab3.cli :as cli]
            [lab3.io :as io]
            [lab3.stream :as stream])
  (:gen-class))

(defn run-interpolation
  "Обрабатывает точки лениво, состояние передаётся явно через reduce"
  [{:keys [algorithms step window-size]} points]
  (let [;; Начальные состояния процессоров (чистые данные)
        initial-states (stream/create-initial-states algorithms window-size)

        ;; Обрабатываем точки через reduce с явной передачей состояния
        ;; Побочный эффект (вывод) выполняется внутри reduce после каждой точки
        final-states (reduce
                      (fn [states point]
                        (let [{:keys [states all-results]}
                              (stream/process-point-all states point step)]
                           ;; Выводим результаты сразу (единственный побочный эффект)
                          (doseq [r all-results]
                            (io/print-result! r))
                          states))
                      initial-states
                      points)

        ;; Финализируем и выводим остаток
        final-results (stream/finalize-all final-states step)]
    (doseq [r final-results]
      (io/print-result! r))))

(defn -main [& args]
  (let [{:keys [ok? exit-message options]} (cli/parse-cli-args args)]
    (if exit-message
      (do
        (println exit-message)
        (System/exit (if ok? 0 1)))
      (try
        (let [algorithms (cli/get-selected-algorithms options)
              {:keys [step window-size delimiter]} options
              lines (io/create-input-stream)
              points (io/validate-sorted (io/parse-points delimiter lines))]
          (run-interpolation {:algorithms algorithms
                              :step step
                              :window-size window-size}
                             points))
        (catch clojure.lang.ExceptionInfo e
          (binding [*out* *err*]
            (println "Error:" (.getMessage e))
            (when-let [data (ex-data e)]
              (println "Details:" data)))
          (System/exit 1))
        (catch Exception e
          (binding [*out* *err*]
            (println "Unexpected error:" (.getMessage e)))
          (System/exit 1))))))
