(ns lab3.core
  (:require [lab3.cli :as cli]
            [lab3.io :as io]
            [lab3.stream :as stream])
  (:gen-class)
  (:import (clojure.lang ExceptionInfo)))

(defn run-interpolation
  "Обрабатывает точки потоково, возвращает последовательность результатов."
  [{:keys [algorithms step window-size]} points]
  (stream/process-all-points algorithms window-size step points))

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
              results (->> (io/create-input-stream)
                           (io/parse-points delimiter)
                           io/validate-sorted
                           (run-interpolation {:algorithms algorithms
                                               :step step
                                               :window-size window-size}))]
          {:exit-code 0 :results results})
        (catch ExceptionInfo e
          {:exit-code 1 :error (format-error "Error:" e)})
        (catch Exception e
          {:exit-code 1 :error (format-error "Unexpected error:" e)})))))

(defn -main
  "Точка входа: единственное место с побочными эффектами (печать, System/exit)"
  [& args]
  (let [{:keys [exit-code output results error]} (run-app args)]
    (cond
      output (println output)
      error (binding [*out* *err*] (println error))
      results (io/print-results! results))
    (System/exit exit-code)))
