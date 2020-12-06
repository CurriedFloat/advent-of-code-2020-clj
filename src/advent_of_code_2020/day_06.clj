(ns advent-of-code-2020.day-06
  (:require [clojure.string :as str]
            [clojure.set :as s]))

(def input (-> (slurp "resources/day_06/input")
               (str/split #"\r\n\r\n")))

;; part 1
(->> input
     (map #(clojure.string/replace % #"\r\n" ""))
     (map distinct)
     (map count)
     (reduce +))

;; part 2
(defn count-group [g]
  (->> (str/split g #"\r\n")
       (map set)
       (apply s/intersection)
       count))

(->> input
     (map count-group)
     (reduce +))
