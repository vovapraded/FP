(ns lab1.problem8.s4
  (:require [lab1.problem8.utils :refer [product]]))

(defn max-product-loop [digits window-size]
  (cond
    (<= window-size 0) ##-Inf
    (< (count digits) window-size) ##-Inf
    :else
    (let [max-prod (atom ##-Inf)]
      (dotimes [i (inc (- (count digits) window-size))]
        (let [window (take window-size (drop i digits))
              prod (product window)]
          (swap! max-prod max prod)))
      @max-prod)))
