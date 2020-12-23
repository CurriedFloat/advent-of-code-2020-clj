(ns advent-of-code-2020.day-17
  (:require [clojure.string :as str]
            [clojure.set :as s]))

(def input (-> (slurp "resources/day_17/input")
               (str/split #"\r\n")))

(defn parse-row [idx row]
  (->> (map-indexed #(vector idx %1 %2) row)
       (keep #(if (= (% 2) \#) (vector (% 0) (% 1) 0 0) nil))))

(def active-cells (->> input
                       (map-indexed parse-row)
                       (reduce #(concat %1 %2) [])
                       set))

(defn spawn? [n] (= n 3))

(defn die? [n] (not (or (= n 2) (= n 3))))

(def neighbors 
  (for [x [-1 0 1]
        y [-1 0 1]
        z [-1 0 1]
        w [-1 0 1]
        :when (not (and (= x 0) (= y 0) (= z 0) (= w 0)))]
    [x y z w]))

(defn add [p1 p2]
  (vec (map + p1 p2)))

(defn pos->neighbors [pos]
  (set (map #(add pos %) neighbors)))

(def m-pos->neighbors (memoize pos->neighbors))

(defn count-active [active-cells neighbors]
  (count (s/intersection active-cells neighbors)))

(defn rule [rule? active-cells cells]
  (->> cells
       (keep #(when (->> (m-pos->neighbors %)
                         (count-active active-cells)
                         rule?) %))
       set))

(defn generation [active-cells]
  (let [all-neighbour-cells (->> (map m-pos->neighbors active-cells)
                                 (apply s/union))
        inactive-cells (s/difference all-neighbour-cells active-cells)]
    (-> active-cells
        (s/difference (rule die? active-cells active-cells))
        (s/union (rule spawn? active-cells inactive-cells)))))

;; part 2
(count (nth (iterate generation active-cells) 6))
