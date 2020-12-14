(ns advent-of-code-2020.day-12
  (:require [clojure.string :as str]))

(def input (-> (slurp "resources/day_12/input")
               (str/split #"\r\n")))

(defn parse [line]
  (let [[cmd num] (map #(apply str %) (split-at 1 line))]
    {:cmd (keyword cmd) :num (read-string num)}))

(defn manhattan-distance [{:keys [E N]}]
  (+ (Math/abs E) (Math/abs N)))

(def clockwise [:N :E :S :W])
(def idx-map {:N 0 :E 1 :S 2 :W 3})

(defn turn [num]
  (fn [dir]
    (let [x (/ num 90)]
      (clockwise (mod (+ (idx-map dir) x) 4)))))

(defn cmd-handel-part-1 
  [s {:keys [cmd num]}]
  (cond
    (= cmd :N) (update s :N #(+ % num))
    (= cmd :S) (update s :N #(- % num))
    (= cmd :E) (update s :E #(+ % num))
    (= cmd :W) (update s :E #(- % num))
    (= cmd :L) (update s :facing (turn (* -1 num)))
    (= cmd :R) (update s :facing (turn num))
    (= cmd :F) (cmd-handel-part-1 s {:cmd (s :facing) :num num})
    :else s))

;; part 1
(->> input
     (map parse)
     (reduce cmd-handel-part-1 {:facing :E :E 0 :N 0})
     manhattan-distance)

;; part 2
(defn rot-90-R  [{:keys [E N]}]
  {:E N :N (* E -1)})

(defn rot-90-L [{:keys [E N]}]
  {:E (* N -1) :N E})

(defn n-times [n f]
  (apply comp (repeat n f)))

(defn cmd-handel-part-2
  [s {:keys [cmd num]}]
  (cond
    (= cmd :N) (update-in s [:wp :N] #(+ % num))
    (= cmd :S) (update-in s [:wp :N] #(- % num))
    (= cmd :E) (update-in s [:wp :E] #(+ % num))
    (= cmd :W) (update-in s [:wp :E] #(- % num))
    (= cmd :L) (-> s (update-in [:wp] #((n-times (/ num 90) rot-90-L) %)))
    (= cmd :R) (-> s (update-in [:wp] #((n-times (/ num 90) rot-90-R) %)))
    (= cmd :F) (-> s
                      (update-in [:ship :N] #(+ % (* num ((s :wp) :N))))
                      (update-in [:ship :E] #(+ % (* num ((s :wp) :E)))))
    :else s))

(->> input
     (map parse)
     (reduce cmd-handel-part-2 {:ship {:E 0 :N 0} 
                                :wp {:E 10 :N 1}}) 
     ((comp manhattan-distance :ship)))
