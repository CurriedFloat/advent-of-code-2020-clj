(ns advent-of-code-2020.day-01
  (:require [advent-of-code-2020.core :as utils]))

(def input (utils/read-lines "resources/day_01/input"))

(defn cartesian-product [colls]
  (if (empty? colls)
    [[]]
    (for [more (cartesian-product (rest colls))
          x (first colls)]
      (conj more x))))

(defn solve [x entries]
  (->> entries
       (some #(if (= x (reduce + %)) (reduce * %) nil))))

;; part 1
(->> (cartesian-product [input input])
     (solve 2020))

;; part 2
(->> (cartesian-product [input input input])
     (solve 2020))