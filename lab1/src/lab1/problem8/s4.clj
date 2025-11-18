(ns lab1.problem8.s4
  (:require [lab1.problem8.utils :refer [product]]))

(defn max-product-loop [digits window-size]
  (cond
    (<= window-size 0) Double/NEGATIVE_INFINITY
    (< (count digits) window-size) Double/NEGATIVE_INFINITY
    :else
    (let [max-prod (atom Double/NEGATIVE_INFINITY)]
      (dotimes [i (inc (- (count digits) window-size))]
        (let [window (take window-size (drop i digits))
              prod (product window)]
          (swap! max-prod max prod)))
      @max-prod)))
