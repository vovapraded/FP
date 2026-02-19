(ns lab3.io
  (:require [clojure.string :as str])
  (:import [java.io BufferedReader]))

(defn create-input-stream []
  (line-seq (BufferedReader. *in*)))

(defn parse-line
  "Returns nil if line is invalid"
  [delimiter line]
  (try
    (let [trimmed (str/trim line)]
      (when-not (or (empty? trimmed)
                    (str/starts-with? trimmed "#"))
        (let [parts (str/split trimmed (re-pattern delimiter))]
          (when (= 2 (count parts))
            {:x (-> parts first str/trim Double/parseDouble)
             :y (-> parts second str/trim Double/parseDouble)}))))
    (catch Exception e
      (binding [*out* *err*]
        (println "Warning: failed to parse line:" line)
        (println "Error:" (.getMessage e)))
      nil)))

(defn parse-points [delimiter lines]
  (->> lines (keep (partial parse-line delimiter))))

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

(defn spy
  ([value]
   (println value)
   value)
  ([label value]
   (println (str label ": " (pr-str value)))
   value))
