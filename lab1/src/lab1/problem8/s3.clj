(ns lab1.problem8.s3
  (:require [lab1.problem8.utils :refer [product]]))

(defn sliding-window [window-size coll]
  (map #(take window-size (drop % coll))
       (range (- (count coll) (dec window-size)))))

(defn max-product-map-generation [digits window-size]
  (cond
    (<= window-size 0) ##-Inf
    (< (count digits) window-size) ##-Inf
    :else
    (let [windows (sliding-window window-size digits)
          products (map product windows)]
      (if (empty? products)
        ##-Inf
        (reduce max products)))))

