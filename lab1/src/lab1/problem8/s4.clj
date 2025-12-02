(ns lab1.problem8.s4
  (:require [lab1.problem8.utils :refer [product]]))

(defn max-product-loop [digits window-size]
  (cond
    (<= window-size 0) Double/NEGATIVE_INFINITY
    (< (count digits) window-size) Double/NEGATIVE_INFINITY
    :else
    (apply max
           (for [i (range (inc (- (count digits) window-size)))]
             (let [window (take window-size (drop i digits))]
               (product window))))))
