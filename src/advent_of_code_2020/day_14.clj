(ns advent-of-code-2020.day-14
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))


(def input (-> (slurp "resources/day_14/input")
               (str/split #"\r\n")))

(defn map-read-string [s]
  (map read-string s))

(defn parse [line]
  (-> line
      (str/replace "[" " ")
      (str/replace "] " "")
      (str/replace "=" "")
      (str/split #"\s+")
      (#(hash-map :i (keyword (first %)) :args (rest %)))
      (#(if (= :mem (% :i)) 
          (update % :args map-read-string)
          %))))

(defn combinations [n]
  (println "combinations" n)
  (apply combo/cartesian-product (repeat n [\1 \0])))

(def combinations-memo (memoize combinations))

(defn bin-str->big-int [s]
  (new java.math.BigInteger s 2))

(defn num->padded-bin-str [n]
  (->> n
       Integer/toBinaryString
       biginteger
       (format "%036d")))

(defn bit-mask [v b]
  (cond
    (= b \0) \0
    (= b \1) \1
    :else v))

(defn mask-value [mask value]
  (->> (map bit-mask (num->padded-bin-str value) mask)
       (apply str)
       bin-str->big-int))

(defn hdl-part-1
  [s {:keys [i args]}]
  (cond
    (= i :mask) (assoc s :mask (first args))
    (= i :mem) (assoc-in s 
                         [:mem (first args)] 
                         (mask-value (s :mask) (last args)))
    :else s))

;; part 1
(->> input
     (map parse)
     (reduce hdl-part-1 {})
     ((comp #(apply + %) vals :mem)))

;; part 2
(defn bit-mask-part-2 [v b]
  (cond
    (= b \1) \1
    :else v))

(defn floating-permutation [address idxs]
  (fn [bin-combination]
    (->> (map vector idxs bin-combination)
         (reduce #(assoc %1 (%2 0) (%2 1)) address)
         (apply str)
         bin-str->big-int)))

(defn all-addresses [mask address]
  (let [padded-address (num->padded-bin-str address)
        stable-address (vec (map bit-mask-part-2 padded-address mask))
        idxs (keep-indexed #(when (= \X %2) %1) mask)
        all-possible-values (combinations-memo (count idxs))]
    (->> all-possible-values
         (map (floating-permutation stable-address idxs)))))

(defn hdl-part-2
  [s {:keys [i args]}]
  (cond
    (= i :mask) (assoc s :mask (first args))
    (= i :mem) (->> (all-addresses (s :mask) (first args))
                    (reduce #(assoc-in %1 [:mem %2] (last args)) s))
    :else s))

(->> input
     (map parse)
     (reduce hdl-part-2 {})
     ((comp #(apply + %) vals :mem)))