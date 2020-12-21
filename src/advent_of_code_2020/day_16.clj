(ns advent-of-code-2020.day-16
  (:require [clojure.string :as str]
            [clojure.set :as s]))

(def input (-> (slurp "resources/day_16/input")
               (str/split #"\r\n\r\n")))

(defn parse-ticket [t] (map read-string (str/split t #",")))

(defn between? [a b] (fn [x] (and (>= x a) (<= x b))))

(defn parse-rule [r]
  (let [[field ranges] (str/split r #": ")]
    [(keyword (str/replace field #" " "-")) 
     (->> (str/split ranges #" or ")
          (map #(map read-string (str/split % #"-")))
          (map #(apply between? %))
          (apply juxt))]))

(def s 
  (let [[rules ticket tickets] input]
    {:rules (into {} (map parse-rule (str/split rules #"\r\n")))
     :my-ticket (parse-ticket (last (str/split ticket #"\r\n")))
     :nearby-tickets (map parse-ticket (rest (str/split tickets #"\r\n")))}))

(defn valid? [s] (not-every? false? s))

(def all-rules (->> s :rules vals 
                    (map #(comp valid? %))
                    (apply juxt)))

(defn invalid-ticket-fields [ticket]
  (keep #(when ((comp not valid? all-rules) %) %) ticket))

;; part 1
(->> (:nearby-tickets s)
     (mapcat invalid-ticket-fields)
     (apply +))

;; part 2
(defn transpose [xs]
  (apply mapv vector xs))

(defn all-valid? [rule-f col]
    (->> col
       (map (comp valid? rule-f))
       (every? true?)))

(defn solve [acc possible-fields rule-name]
  (let [field (s/difference (set possible-fields) (:solved-fields acc))]
    (-> acc
        (update :solved-fields #(s/union % field))
        (update :rule->field #(conj % [rule-name (first field)])))))

(def valid-tickets (->> (:nearby-tickets s)
                        (filter #(empty? (invalid-ticket-fields %)))))

(->> (for [[rule-name rule-f] (:rules s)
           [idx col] (->> valid-tickets transpose (map-indexed vector))
           :when (all-valid? rule-f col)]
       (hash-map rule-name [idx]))
     (apply merge-with into)
     s/map-invert
     (into (sorted-map))
     (reduce-kv solve {:solved-fields #{}
                       :rule->field {}})
     :rule->field
     ((juxt :departure-platform
            :departure-time
            :departure-station
            :departure-track
            :departure-location
            :departure-date))
     (map (vec (:my-ticket s)))
     (apply *))