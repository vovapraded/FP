(ns lab1.names-scores.modular)

(defn calculate-name-score [name-str]
  (reduce + (map #(inc (- (int %) (int \A))) name-str)))

(defn total-score [names]
  (let [sorted-names (sort names)]
    (reduce + (map-indexed #(* (inc %1) (calculate-name-score %2)) sorted-names))))
