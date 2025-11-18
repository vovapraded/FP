(ns lab1.problem8.s2
  (:require [lab1.problem8.utils :refer [product]]))

(defn sliding-window [window-size coll]
  (partition window-size 1 coll))

(defn max-product-modular [digits window-size]
  (cond
    (<= window-size 0) Double/NEGATIVE_INFINITY
    (< (count digits) window-size) Double/NEGATIVE_INFINITY
    :else
    (let [windows (sliding-window window-size digits)
          products (map product windows)]
      (if (empty? products)
        Double/NEGATIVE_INFINITY
        (reduce max products)))))
