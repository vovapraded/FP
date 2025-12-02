(ns lab1.names-scores.tail-recursive)

(defn calculate-name-score [name-str]
  (letfn [(letter-value [letters acc]
            (if (empty? letters)
              acc
              (recur (rest letters)
                     (+ acc (inc (- (int (first letters)) (int \A)))))))]
    (letter-value name-str 0)))

(defn total-score [names]
  (let [sorted-names (sort names)]
    (letfn [(score-with-positions [name-list position acc]
              (if (empty? name-list)
                acc
                (let [score (* position (calculate-name-score (first name-list)))]
                  (recur (rest name-list) (inc position) (+ acc score)))))]
      (score-with-positions sorted-names 1 0))))
