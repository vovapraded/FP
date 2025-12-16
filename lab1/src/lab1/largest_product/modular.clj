(ns lab1.largest-product.modular
  (:require [lab1.largest-product.utils :refer [product]]))

(defn sliding-window [window-size coll]
  (partition window-size 1 coll))

(defn max-product [digits window-size]
  (cond
    (<= window-size 0) Double/NEGATIVE_INFINITY
    (< (count digits) window-size) Double/NEGATIVE_INFINITY
    :else
    (let [windows (sliding-window window-size digits)
          products (map product windows)]
      (reduce max products))))
