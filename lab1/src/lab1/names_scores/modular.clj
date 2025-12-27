(ns lab1.names-scores.modular)

(defn calculate-name-score [name-str]
  (->> name-str
       (map #(inc (- (int %) (int \A))))
       (reduce +)))

(defn total-score [names]
  (->> names
       sort
       (map-indexed #(* (inc %1) (calculate-name-score %2)))
       (reduce +)))
