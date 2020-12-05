(ns advent-of-code-2020.day-03
  (:require [clojure.string :as str]))

(def input (-> (slurp "resources/day_03/input")
               (str/split #"\r\n")))

;; solved using cycle instead of a wrap around index, not efficient... but fun!
(def map-of-area (->> input
                      (map-indexed (fn [idx itm] [idx (cycle itm)]))
                      (drop 1)))

(def slopes [{:right 1 :down 1}
             {:right 3 :down 1}
             {:right 5 :down 1}
             {:right 7 :down 1}
             {:right 1 :down 2}])

(defn tree? [x]
  (= \# x))

(defn move-down [{:keys [down]}]
  (fn [x]
    (let [[idx] x]
      (= 0 (mod idx down)))))

(defn move-right [{:keys [right down]}]
  (fn [x]
    (let [[idx row] x]
      (nth row (* right (/ idx down))))))

(defn path [slope lines] 
  (->> lines
       (filter (move-down slope))
       (map (move-right slope))))

(defn trees-in-path [slope]
  (fn [lines]
    (->> lines
         (path slope)
         (filter tree?)
         count)))

;; part 1
((trees-in-path (slopes 1)) map-of-area)

;; part 2
(->> ((apply juxt (map trees-in-path slopes)) map-of-area)
     (reduce *))

