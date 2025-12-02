(ns lab1.names-scores.loop)

(defn calculate-name-score [name-str]
  (reduce +
          (for [char name-str]
            (inc (- (int char) (int \A))))))

(defn total-score [names]
  (let [sorted-names (sort names)]
    (reduce +
            (for [i (range (count sorted-names))]
              (* (inc i) (calculate-name-score (nth sorted-names i)))))))
