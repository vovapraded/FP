(ns lab1.problem8.s1-2
  (:require [lab1.problem8.utils :refer [product]]))

(defn max-product-recursion [coll window-size]
  (cond
    (<= window-size 0) Double/NEGATIVE_INFINITY
    (< (count coll) window-size) Double/NEGATIVE_INFINITY
    (= (count coll) window-size) (product coll)
    :else
    (max
     (product (take window-size coll))
     (max-product-recursion (next coll) window-size))))