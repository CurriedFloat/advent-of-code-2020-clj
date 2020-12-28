(ns advent-of-code-2020.day-24
  (:require [clojure.string :as str]
            [clojure.set :as s]))

(def input (-> (slurp "resources/day_24/input")
               (str/split #"\r\n")))

;; Cube coordinates https://www.redblobgames.com/grids/hexagons/
(defn direction [x]
  (case x
    :e  [1 -1 0]
    :se [0 -1 1]
    :sw [-1 0 1]
    :w  [-1 1 0]
    :nw [0 1 -1]
    :ne [1 0 -1]
    [0 0 0]))

(defn add [p1 p2]
  (vec (map + p1 p2)))

(defn flip [s tile]
  (update s tile not))

(defn move [x]
  (->> (re-seq #"e|se|sw|w|nw|ne" x)
       (map keyword)
       (map direction)
       (reduce add [0 0 0])))

(def black-tiles (->> input
                (map move)
                (reduce flip {})
                (keep #(when (true? (% 1)) (% 0)))
                set))

;; part 1
(count black-tiles)

;; part 2
(defn neighbors [tile]
  (->>  [:e :se :sw :w :nw :ne]
        (map direction)
        (map #(add tile %))))

(def neighbors-m (memoize neighbors))

(defn only-neighbors [tiles]
  (->> tiles
       (mapcat neighbors-m)
       set
       (#(s/difference % tiles))))

(defn black? [x] 
  (or (= 0 x) (> x 2)))

(defn white? [x]
  (= 2 x))

(defn flip? [pred?]
  (fn [x black-tiles]
    (->> (map black-tiles (neighbors-m x))
         (remove nil?)
         count
         pred?)))

(def flip-black? (flip? black?))
(def flip-white? (flip? white?))

(defn turn [black-tiles]
  (let [neighbors (->> black-tiles only-neighbors)
        black-tiles-to-flip (->> black-tiles 
                                 (filter #(flip-black? % black-tiles)) 
                                 set)
        white-tiles-to-flip (->> neighbors 
                                 (filter #(flip-white? % black-tiles)) 
                                 set)]
    (-> black-tiles
        (s/union white-tiles-to-flip)
        (s/difference black-tiles-to-flip))))

(count (nth (iterate turn black-tiles) 100))
