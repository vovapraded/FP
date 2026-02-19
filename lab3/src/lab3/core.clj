(ns lab3.core
  (:require [lab3.cli :as cli]
            [lab3.io :as io]
            [lab3.stream :as stream])
  (:gen-class)
  (:import (clojure.lang ExceptionInfo)))

(defn- print-results! [results]
  (run! io/print-result! results))

(defn- process-point [step states point]
  (let [{:keys [states all-results]} (stream/process-point-all states point step)]
    (print-results! all-results)
    states))

(defn run-interpolation
  "Обрабатывает точки потоково, состояние передаётся явно через reduce"
  [{:keys [algorithms step window-size]} points]
  (-> (reduce (partial process-point step)
              (stream/create-initial-states algorithms window-size)
              points)
      (stream/finalize-all step)
      print-results!))

(defn- handle-error! [prefix ^Exception e]
  (binding [*out* *err*]
    (println prefix (.getMessage e))
    (some->> (ex-data e) (println "Details:")))
  (System/exit 1))

(defn -main [& args]
  (let [{:keys [ok? exit-message options]} (cli/parse-cli-args args)]
    (if exit-message
      (do (println exit-message)
          (System/exit (if ok? 0 1)))
      (try
        (let [{:keys [step window-size delimiter]} options
              algorithms (cli/get-selected-algorithms options)]
          (->> (io/create-input-stream)
               (io/parse-points delimiter)
               io/validate-sorted
               (run-interpolation {:algorithms algorithms
                                   :step step
                                   :window-size window-size})))
        (catch ExceptionInfo e (handle-error! "Error:" e))
        (catch Exception e (handle-error! "Unexpected error:" e))))))
