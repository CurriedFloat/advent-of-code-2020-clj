(ns advent-of-code-2020.day-04
  (:require [clojure.string :as str]
            [clojure.spec.alpha :as s]))

(def input (-> (slurp "resources/day_04/input")
               (str/split #"\r\n\r\n")))

(defn str->int [number-string]
  (try (Integer/parseInt number-string)
       (catch Exception _ nil)))
(defn num? [x] (not (nil? (str->int x))))
(defn count? [n x] (= n (count x)))
(defn between? [start stop x] (and (<= start x) (>= stop x)))
(defn year-between? [start stop x]
  (and (num? x) (count? 4 x) (between? start stop (str->int x))))

;; year specs
(s/def ::byr #(year-between? 1920 2002 %))
(s/def ::iyr #(year-between? 2010 2020 %))
(s/def ::eyr #(year-between? 2020 2030 %))
;; height spec
(def height-reg #"([0-9]+)(cm|in)")
(defn height? [x]
  (let  [[_ h-str t] (re-matches height-reg x)
         h (str->int h-str)]
    (cond
      (= t "cm") (between? 150 193 h)
      (= t "in") (between? 59 76 h)
      :else false)))
(s/def ::hgt (s/and #(re-matches height-reg %) height?))
;; color specs
(def hex-color-reg  #"^#(?:[0-9a-fA-F]{6}){1,2}$")
(s/def ::hcl #(re-matches hex-color-reg %))
(s/def ::ecl #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})

(s/def ::pid (s/and num? #(count? 9 %)))

(s/def ::passport (s/keys :req [::byr ::iyr ::eyr ::hgt ::hcl ::ecl ::pid]
                          :opt [::cid]))

(defn str->ns-keyword [x]
  (keyword (str *ns*) x))

(defn parse-passport [passport-text]
  (->> (str/split passport-text #"\r\n|\n| ")
       (map #(str/split % #":"))
       (map #(update % 0 str->ns-keyword))
       (into (sorted-map))))

;; part 2
(->> input
     (map parse-passport)
     (filter #(s/valid? ::passport %))
     count)

