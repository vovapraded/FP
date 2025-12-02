(ns lab1.largest-product.recursive
  (:require [lab1.largest-product.utils :refer [product]]))

(defn max-product [coll window-size]
  (cond
    (<= window-size 0) Double/NEGATIVE_INFINITY
    (< (count coll) window-size) Double/NEGATIVE_INFINITY
    (= (count coll) window-size) (product coll)
    :else
    (max
     (product (take window-size coll))
     (max-product (next coll) window-size))))