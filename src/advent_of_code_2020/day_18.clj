(ns advent-of-code-2020.day-18
  (:require [clojure.string :as str]
            [clojure.walk :as w]))

(def input (-> (slurp "resources/day_18/input")
               (str/split #"\r\n")))

(defn parse-expr [expr]
  (->> (str/join "" ["(" expr ")"])
       read-string))
  
(defn in-order [acc node]
  (if (ifn? node)
    (assoc acc :op node)
    (if (not (nil? (:val acc)))
      (assoc acc :val ((resolve (:op acc)) (:val acc) node))
      (assoc acc :val node))))

;; part 1
(->> input
     (map parse-expr)
     (map #(w/postwalk (fn [node]
                         (if (seq? node)
                           ((reduce in-order {} node) :val)
                           node)) %))
     (apply +))

;; part 2
(defn +? [x] (= #'clojure.core/+ (resolve x)))

(defn add-first [node]
  (->> node
       (partition-by #(or (number? %) (+? %)))
       (map #(if (> (count %) 1)
               ((reduce in-order {} %) :val) 
               %))
       flatten))

(->> input
     (map parse-expr)
     (map #(w/postwalk (fn [node]
                         (if (seq? node)
                           ((reduce in-order {} (add-first node)) :val) 
                           node)) %))
     (apply +))

