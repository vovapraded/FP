(ns lab1.largest-product.loop
  (:require [lab1.largest-product.utils :refer [product]]))

(defn max-product [digits window-size]
  (cond
    (<= window-size 0) Double/NEGATIVE_INFINITY
    (< (count digits) window-size) Double/NEGATIVE_INFINITY
    :else
    (apply max
           (for [i (range (inc (- (count digits) window-size)))]
             (let [window (take window-size (drop i digits))]
               (product window))))))
