(ns advent-of-code-2020.day-02
  (:require [clojure.string :as str]
            [clojure.test]))

(def input (-> (slurp "resources/day_02/input")
               (str/split #"\r\n")))

(defn parse-line 
  {:test #(do (assert (= (parse-line "1-3 b: cdefg") 
                         {:nums '(1 3) :char \b :pw "cdefg"})))}
  [line]
  (let [[nums char pw] (str/split line #" ")]
    {:nums (map read-string (str/split nums #"-")) 
     :char (first char) 
     :pw pw}))

(test #'parse-line)

;; part 1
(defn check-lim-policy? [{:keys [nums char pw]}]
  (let [[low high] nums
        v ((frequencies pw) char)]
    (and (some? v) 
         (>= v low) 
         (<= v high))))

(->> input
     (map parse-line)
     (filter check-lim-policy?)
     count)

;; part 2
(defn xor [a b] 
  (or (and a (not b)) (and (not a) b)))

(defn check-pos-policy? [{:keys [nums char pw]}]
  (->> nums
       (map dec)
       (map #(get pw %))
       (map #(= char %))
       (apply xor)))

(->> input
     (map parse-line)
     (filter check-pos-policy?)
     count)