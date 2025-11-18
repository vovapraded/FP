(ns lab1.problem22.s1-1)

(defn names-score [names]
  (let [sorted-names (sort names)]))

(defn get-names-score [letters acc]
  (letfn [(get-score [letter] (- (int letter) (int \A)))]
    (if (empty? letters)
      acc
      (recur (rest letters) (+ acc (get-score (first letters)))))))



