(ns advent-of-code-2020.day-10
  (:require [clojure.string :as str]))

(def input (->> (-> (slurp "resources/day_10/input")
                    (str/split #"\r\n"))
                (map read-string)))

;; part 1
(->> input
     sort
     (#(conj % 0))
     (partition 2 1)
     (map #(apply - %))
     (frequencies)
     (#(update % -3 inc))
     (vals)
     (apply *))

;; part 2
(defn get-or-zero [s n]
    (let [x (s n)]
     (if (nil? x) 0 x)))

(defn f [acc curr]
  (assoc acc curr (+ (get-or-zero acc (- curr 1))
                     (get-or-zero acc (- curr 2))
                     (get-or-zero acc (- curr 3)))))

(->> input
     sort
     (reduce f (sorted-map 0 1))
     ((comp last last)))     
