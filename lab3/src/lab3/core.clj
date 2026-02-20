(ns lab3.core
  (:require [lab3.cli :as cli]
            [lab3.io :as io]
            [lab3.stream :as stream])
  (:gen-class)
  (:import (clojure.lang ExceptionInfo)))

(defn run-interpolation-streaming!
  "Обрабатывает точки потоково с выводом результатов."
  [{:keys [algorithms step window-size]} points]
  (loop [state (stream/initial-state algorithms window-size)
         remaining points]
    (if-let [point (first remaining)]
      (let [{:keys [state results]} (stream/step-processor state point step)]
        (when results
          (doseq [r results]
            (io/print-result! r)))
        (recur state (rest remaining)))
      ;; Вывод оставшихся точек
      (when-let [final-results (stream/finalize-processor state step)]
        (doseq [r final-results]
          (io/print-result! r))))))

(defn- format-error
  [prefix ^Exception e]
  (let [base-msg (str prefix " " (.getMessage e))
        details (ex-data e)]
    (if details
      (str base-msg "\nDetails: " (pr-str details))
      base-msg)))

(defn- run-app
  "Основная логика приложения, возвращает {:exit-code :output :error}"
  [args]
  (let [{:keys [ok? exit-message options]} (cli/parse-cli-args args)]
    (cond
      exit-message
      {:exit-code (if ok? 0 1) :output exit-message}

      :else
      (try
        (let [{:keys [step window-size delimiter]} options
              algorithms (cli/get-selected-algorithms options)
              points (->> (io/create-input-stream)
                          (io/parse-points delimiter)
                          io/validate-sorted)]
          (run-interpolation-streaming! {:algorithms algorithms
                                         :step step
                                         :window-size window-size}
                                        points)
          {:exit-code 0})
        (catch ExceptionInfo e
          {:exit-code 1 :error (format-error "Error:" e)})
        (catch Exception e
          {:exit-code 1 :error (format-error "Unexpected error:" e)})))))

(defn -main
  [& args]
  (let [{:keys [exit-code output error]} (run-app args)]
    (cond
      output (println output)
      error (binding [*out* *err*] (println error)))
    (System/exit exit-code)))
