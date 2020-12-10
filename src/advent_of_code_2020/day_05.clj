(ns advent-of-code-2020.day-05
  (:require [clojure.string :as str]
            [clojure.set :as s]))

(def input (-> (slurp "resources/day_05/input")
               (str/split #"\r\n")))

(defn seat-id [{:keys [col row]}]
  (+ col (* 8 row)))

(def rows (range 128))
(def cols (range 8))

(defn search [bin?]
  (fn [s x]
    (if (bin? x)
      (drop (/ (count s) 2) s)
      (take (/ (count s) 2) s))))

(defn find-seat [seat-text]
  (let [[row col] (split-at 7 seat-text)]
    {:row (first (reduce (search #(= \B %)) rows row))
     :col (first (reduce (search #(= \R %)) cols col))}))

;; part 1
(->> input
     (map find-seat)
     (map seat-id)
     (apply max))

;; part 2
(defn eliminate [sorted-seats]
  (let [all (set (range (first sorted-seats) 
                        (last sorted-seats)))]
    (first (s/difference all sorted-seats))))

(->> input
     (map find-seat)
     (map seat-id)
     (apply sorted-set)
     eliminate)