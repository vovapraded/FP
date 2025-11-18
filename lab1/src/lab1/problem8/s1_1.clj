(ns lab1.problem8.s1-1
  (:require [lab1.problem8.utils :refer [product]]))

(defn max-product-tail-recursion
  ([coll window-size]
   (cond
     (<= window-size 0) ##-Inf
     (< (count coll) window-size) ##-Inf
     :else
     (max-product-tail-recursion ##-Inf coll window-size)))

  ([current-max-product rest window-size]
   (if (< (count rest) window-size)
     current-max-product
     (recur
      (max current-max-product (product (take window-size rest)))
      (next rest)
      window-size))))