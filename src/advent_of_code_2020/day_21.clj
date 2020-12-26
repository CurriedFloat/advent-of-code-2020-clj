(ns advent-of-code-2020.day-21
  (:require [clojure.string :as str]
            [clojure.walk :as w]
            [clojure.set :as s]))

(def input (-> (slurp "resources/day_21/input")
               (str/replace #"contains" "")
               (str/split #"\r\n")))

(defn parse-food [x]
  (->> (read-string (str/join ["(" x ")"]))
       (w/postwalk #(if (symbol? %) (keyword  %) %))))

(def all-ingredients
  (->> input
       (map parse-food)
       (map reverse)
       (map rest)
       flatten))

(def allergen->possible-ingredients 
  (->> input
       (map parse-food)
       (map reverse)
       (map (fn [x]
              (let [[allergens & ingredients] x]
                (reduce #(conj %1 [%2 (set ingredients)]) {} allergens))))
       (#(apply merge-with s/intersection %))))


(def has-allergens 
  (->> allergen->possible-ingredients
       (reduce #(s/union %1 (%2 1)) #{})))

;; part 1
(->> (s/difference (set all-ingredients) has-allergens)
     (map (frequencies all-ingredients))
     (apply +))

;; part 2
(defn solve-ingredients [ingredient->allergen x]
  (let [[possible-ingredients allergen] x
        ingredient (->> possible-ingredients
                        (drop-while #(contains? ingredient->allergen %))
                        first)]
    (assoc ingredient->allergen ingredient allergen)))

(->> allergen->possible-ingredients
     (s/map-invert)
     (reduce #(into %1 [(conj %2 (count (%2 0)))]) [])
     (sort #(compare (last %1) (last %2)))
     (reduce solve-ingredients {})
     s/map-invert
     (into (sorted-map))
     vals
     (map name)
     (str/join ","))
