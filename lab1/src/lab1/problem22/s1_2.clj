(ns lab1.problem22.s1-2)

(defn name-score [name]
  (if (empty? name)
    0
    (+ (+ (- (int (first name)) (int \A)) 1)
       (name-score (rest name)))))


(defn total-names-score [names]
  (let [sorted-names (sort names)]
    (letfn [(score-with-positions [name-list position]
              (if (empty? name-list)
                0
                (+ (* position (name-score (first name-list)))
                   (score-with-positions (rest name-list) (inc position)))))]
      (score-with-positions sorted-names 1))))
