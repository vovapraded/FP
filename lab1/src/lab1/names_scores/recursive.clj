(ns lab1.names-scores.recursive)

(defn calculate-name-score [name-str]
  (if (empty? name-str)
    0
    (+ (inc (- (int (first name-str)) (int \A)))
       (calculate-name-score (rest name-str)))))

(defn total-score [names]
  (let [sorted-names (sort names)]
    (letfn [(score-with-positions [name-list position]
              (if (empty? name-list)
                0
                (+ (* position (calculate-name-score (first name-list)))
                   (score-with-positions (rest name-list) (inc position)))))]
      (score-with-positions sorted-names 1))))
