(ns lab3.io
  (:require [clojure.string :as str])
  (:import (java.io BufferedReader)))

(defn create-input-stream []
  (line-seq (BufferedReader. *in*)))

(defn parse-line
  "Парсит строку в точку {:x :y}. Возвращает nil если строка невалидна.
   delimiter-regex — скомпилированный regex (re-pattern)"
  [delimiter-regex line]
  (when-not (str/blank? line)
    (try
      (let [trimmed (str/trim line)
            [x-str y-str :as parts] (str/split trimmed delimiter-regex)]
        (when (= 2 (count parts))
          {:x (Double/parseDouble (str/trim x-str))
           :y (Double/parseDouble (str/trim y-str))}))
      (catch NumberFormatException _ nil))))

(defn parse-points
  "Парсит строки в точки, логирует невалидные строки с номерами"
  [delimiter lines]
  (let [regex (re-pattern delimiter)]
    (->> lines
         (map-indexed vector)
         (keep (fn [[idx line]]
                 (cond
                   (str/blank? line) nil
                   :else (if-let [point (parse-line regex line)]
                           point
                           (do
                             (binding [*out* *err*]
                               (println "Warning: invalid line" (inc idx) ":" line))
                             nil))))))))

(defn validate-sorted
  "Throws exception on first unsorted or duplicate pair"
  [points]
  (let [check-sorted (fn [prev-x point]
                       (let [curr-x (:x point)]
                         (when (and prev-x (== curr-x prev-x))
                           (throw (ex-info (str "Duplicate x coordinate found "
                                                "(interpolation requires unique x values)")
                                           {:previous prev-x :current curr-x})))
                         (when (and prev-x (< curr-x prev-x))
                           (throw (ex-info "Input points must be sorted by x coordinate"
                                           {:previous prev-x :current curr-x})))
                         curr-x))]
    ;; Use reductions to maintain state, but only realize as consumed
    (map second
         (rest (reductions (fn [[prev-x _] point]
                             [(check-sorted prev-x point) point])
                           [nil nil]
                           points)))))

(defn format-result [algorithm x y]
  (format "%s: %.2f | %.2f" (name algorithm) (double x) (double y)))

(defn print-result!
  ([algorithm x y]
   (println (format-result algorithm x y))
   (flush))
  ([{:keys [algorithm x y]}]
   (print-result! algorithm x y)))

(defn print-results! [results]
  (run! print-result! results))
