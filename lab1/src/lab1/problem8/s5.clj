(ns lab1.problem8.s5
  (:require [lab1.problem8.utils :refer [product]]))

(defn sliding-window [window-size coll]
  (partition window-size 1 coll))

(defn max-product-modular-for-lazy-collections [digits window-size]
  (cond
    (<= window-size 0) ##-Inf
    (< (count digits) window-size) ##-Inf
    :else
    (let [windows (sliding-window window-size digits)
          products (map product windows)]
      (if (empty? products)
        ##-Inf
        (reduce max products)))))
