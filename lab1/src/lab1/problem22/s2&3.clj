(ns lab1.problem22.s2&3)

(defn name-score [name]
  (reduce + (map #(+ (- (int %) (int \A)) 1) name)))


(defn total-names-score [names]
  (let [sorted-names (sort names)]
    (reduce + (map-indexed #(* (inc %1) (name-score %2)) sorted-names))))
