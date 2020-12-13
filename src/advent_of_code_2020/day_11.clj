(ns advent-of-code-2020.day-11
  (:require [clojure.string :as str]))

(def input (-> (slurp "resources/day_11/input")
               (str/split #"\r\n")))

(defn vec-map [f s]
  (vec (map f s)))

(def init-layout (vec-map #(vec-map (comp keyword str) %) input))

(defn get-with-default [s default pos]
  (let [[row col] pos]
    (try ((s row) col) (catch Exception _ default))))

(def positions (for [rows (range (count init-layout))
                     cols (range (count (init-layout 0)))]
                 [rows cols]))

(def dirs [[-1 -1][-1  0][-1  1]
           [ 0 -1]       [ 0  1]
           [ 1 -1][ 1  0][ 1  1]])

(defn move-in-dir [pos dir]
  [(+ (pos 0) (dir 0)) (+ (pos 1) (dir 1))])

(defn adjacencies [layout pos]
  (->> dirs
       (map #(vec (move-in-dir pos %)))
       (map #(get-with-default layout :. %))
       frequencies))

(defn empty-rule [seats]
  (if (not (contains? seats :#))
    :# :L))

(defn occupied-rule [n seats]
   (if (when (contains? seats :#)
         (>= (seats :#) n))
     :L :#))

(defn make-rules [f n]
  (fn [layout pos]
    (let [x (get-with-default layout :. pos)]
      (cond
        (= x :L) (empty-rule (f layout pos))
        (= x :#) (occupied-rule n (f layout pos))
        :else x))))

(defn update-pos [rules-f orig-layout]
  (fn [new-layout curr-pos]
    (assoc-in new-layout curr-pos (rules-f orig-layout curr-pos))))

(defn tick [rules-f]
  (fn [layout] (reduce (update-pos rules-f layout) layout positions)))

(defn ticks
  [rules-f layout] 
  (lazy-seq (cons layout (ticks rules-f ((tick rules-f) layout)))))

(defn stablize [rules-f layout]
  (->> (ticks rules-f layout)
       (partition 2 1)
       (drop-while #(not (apply = %)))
       (take 1)
       ((comp last last))))

;; part 1
(->> (stablize (make-rules adjacencies 4) init-layout)
     flatten
     (filter #(= :# %))
     count)

;; part 2
(defn line-of-sight
  [layout dir pos]
  (lazy-seq (cons (get-with-default layout nil pos)
                  (line-of-sight layout dir (move-in-dir pos dir)))))

(defn floor? [x]
  (= :. x))

(defn in-sight [layout pos]
  (->> dirs
       (map #(->> (line-of-sight layout % pos)
                  (drop 1)
                  (drop-while floor?)
                  (take 1)
                  last))
       frequencies))

(->> (stablize (make-rules in-sight 5) init-layout)
     flatten
     (filter #(= :# %))
     count)
