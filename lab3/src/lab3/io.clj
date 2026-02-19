(ns lab3.io
    (:require [clojure.string :as str])
    (:import [java.io BufferedReader]))

;; ========== INPUT ==========

(defn create-input-stream
    "Create lazy sequence of lines from stdin"
    []
    (line-seq (BufferedReader. *in*)))

(defn parse-line
    "Parse CSV line into point map {:x ... :y ...}
     Returns nil if line is invalid"
    [delimiter line]
    (try
        (let [trimmed (str/trim line)]
            (when-not (or (empty? trimmed)
                          (str/starts-with? trimmed "#"))  ; allow comments
                (let [parts (str/split trimmed (re-pattern delimiter))]
                    (when (= 2 (count parts))
                        (let [x (Double/parseDouble (str/trim (first parts)))
                              y (Double/parseDouble (str/trim (second parts)))]
                            {:x x :y y})))))
        (catch Exception e
            (binding [*out* *err*]
                (println "Warning: failed to parse line:" line)
                (println "Error:" (.getMessage e)))
            nil)))

(defn parse-points
    "Convert lazy sequence of lines to lazy sequence of points
     Filters out nil values from failed parses"
    [delimiter lines]
    (->> lines
         (map (partial parse-line delimiter))
         (filter some?)))

(defn validate-sorted
    "Check that points are sorted by x coordinate
     Returns lazy seq, throws exception on first unsorted pair"
    [points]
    (let [check-sorted (fn [prev-x point]
                           (let [curr-x (:x point)]
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

;; ========== OUTPUT ==========

(defn- format-number [n]
    (if (== n (long n))
        (str (long n))
        (format "%.6f" n)))

(defn format-result
    "Format interpolation result for output"
    [algorithm x y]
    (format "%s: %s %s" (name algorithm) (format-number x) (format-number y)))

(defn print-result!
    "Print single result and flush output immediately"
    ([algorithm x y]
     (println (format-result algorithm x y))
     (flush))
    ([{:keys [algorithm x y]}]
     (print-result! algorithm x y)))

(defn print-results!
    "Print sequence of results
     Each result is a map {:algorithm :x :y}"
    [results]
    (doseq [result results]
        (print-result! result)))

;; ========== DEBUG ==========

(defn spy
    "Debug helper - prints value and returns it
     Can be used with or without label"
    ([value]
     (println value)
     value)
    ([label value]
     (println (str label ": " (pr-str value)))
     value))

