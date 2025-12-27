(ns lab1.largest-product.map-generation
  (:require [lab1.largest-product.utils :refer [product]]))

(defn max-product [digits window-size]
  (cond
    (<= window-size 0) Double/NEGATIVE_INFINITY
    (< (count digits) window-size) Double/NEGATIVE_INFINITY
    :else
    (->> (range (inc (- (count digits) window-size)))
         (map #(take window-size (drop % digits)))
         (map product)
         (apply max))))
