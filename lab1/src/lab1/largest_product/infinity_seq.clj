(ns lab1.largest-product.infinity-seq
  (:require [lab1.largest-product.tail-recursive :refer [max-product]]))

(defn fibonacci-digits-seq
  []
  (map first (iterate (fn [[a b]] [(mod b 10) (mod (+ a b) 10)]) [0 1])))

(defn max-product-from-fibonacci
  [window-size take-count]
  (max-product (-> (fibonacci-digits-seq)
                   (take take-count))
               window-size))