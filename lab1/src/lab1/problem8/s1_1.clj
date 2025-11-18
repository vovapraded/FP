(ns lab1.problem8.s1-1
  (:require [lab1.problem8.utils :refer [product]]))

(defn max-product-tail-recursion
  ([coll window-size]
   (cond
     (<= window-size 0) Double/NEGATIVE_INFINITY
     (< (count coll) window-size) Double/NEGATIVE_INFINITY
     :else
     (max-product-tail-recursion Double/NEGATIVE_INFINITY coll window-size)))

  ([current-max-product remaining window-size]
   (if (< (count remaining) window-size)
     current-max-product
     (recur
      (max current-max-product (product (take window-size remaining)))
      (next remaining)
      window-size))))