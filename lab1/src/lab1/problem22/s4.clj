(ns lab1.problem22.s4)

(defn name-score [name]
  (reduce +
          (for [char name]
            (+ (- (int char) (int \A)) 1))))


(defn total-names-score [names]
  (let [sorted-names (sort names)]
    (reduce +
            (for [i (range (count sorted-names))]
              (* (inc i) (name-score (nth sorted-names i)))))))






