(ns lab3.core
  (:require [lab3.cli :as cli]
            [lab3.io :as io]
            [lab3.stream :as stream])
  (:gen-class))

(defn create-output-fn
  "Создать функцию вывода результата"
  []
  (fn [{:keys [algorithm x y]}]
    (io/print-result! algorithm x y)))

(defn run-interpolation
  "Запустить интерполяцию для заданных параметров
   algorithms - список алгоритмов [:linear :newton ...]
   options - опции из CLI
   points - последовательность точек"
  [algorithms options points]
  (let [{:keys [step window-size]} options
        output-fn (create-output-fn)
        ;; Материализуем точки, чтобы их можно было использовать повторно
        points-vec (vec points)]
    ;; Запускаем каждый алгоритм
    (doseq [algorithm algorithms]
      (let [processor (stream/create-stream-processor algorithm window-size step)]
        (processor points-vec output-fn)))))

(defn -main [& args]
  (let [{:keys [ok? exit-message options]} (cli/parse-cli-args args)]
    (if exit-message
      (do
        (println exit-message)
        (System/exit (if ok? 0 1)))
      (try
        (let [algorithms (cli/get-selected-algorithms options)
              delimiter (:delimiter options)
              lines (io/create-input-stream)
              points (io/validate-sorted (io/parse-points delimiter lines))]
          (run-interpolation algorithms options points))
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
