(ns lab1.problem22.s1-1)

(defn name-score [name]
  (letfn [(letter-value [letters acc]
            (if (empty? letters)
              acc
              (recur (rest letters)
                     (+ acc (+ (- (int (first letters)) (int \A)) 1)))))]
    (letter-value name 0)))

(defn total-names-score [names]
  (let [sorted-names (sort names)]
    (letfn [(score-with-positions [name-list position acc]
              (if (empty? name-list)
                acc
                (let [score (* position (name-score (first name-list)))]
                  (recur (rest name-list) (inc position) (+ acc score)))))]
      (score-with-positions sorted-names 1 0))))
