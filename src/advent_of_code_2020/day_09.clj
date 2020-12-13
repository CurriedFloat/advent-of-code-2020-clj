(ns advent-of-code-2020.day-09
  (:require [clojure.string :as str]))

(def input (->> 
            (-> (slurp "resources/day_09/input") 
                (str/split #"\r\n"))
            (map read-string)))

(defn cartesian-product [colls]
  (if (empty? colls)
    [[]]
    (for [more (cartesian-product (rest colls))
          x (first colls)]
      (conj more x))))

(defn sums [x]
  (->> (cartesian-product [x x])
       (map #(reduce + %))))

(defn valid? [x]
  (let [n (peek x)
        s (sums (pop x))]
    (some #(= n %) s)))

;; part 1
(def invalid-number (->> input
                         (partition 26 1)
                         (map vec)
                         (drop-while valid?)
                         (take 1)
                         ((comp last last))))

;; part 2
(defn find-contiguous-seg [s x]
  (loop [a 0
         b 1]
    (let [sum (reduce + (subvec s a b))
          found (= x sum)
          cont (> x sum)
          end (= b (count s))]
      (if (or found end)
        (subvec s a b)
        (if cont
          (recur a (inc b))
          (recur (inc a) b))))))

(->> (find-contiguous-seg (vec input) invalid-number)
     (#(+ (apply max %) 
          (apply min %))))
