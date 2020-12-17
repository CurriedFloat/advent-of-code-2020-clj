(ns advent-of-code-2020.day-13
  (:require [clojure.string :as str]
            [clojure.math.numeric-tower :as math]))

(def input (-> (slurp "resources/day_13/input")
               (str/split #"\r\n")))

(defn parse [lines]
  (let [[earliest-timestamp schedule] lines]
    [(read-string earliest-timestamp) 
     (->> (str/split schedule #"," )
          (filter #(not= % "x"))
          (map read-string))]))

(def parsed-input (parse input))
(def arrival-time (parsed-input 0))

(defn id->sched [id]
  (->> (range (dec id))
       (map (constantly 0))
       (#(conj % id))
       reverse
       cycle))

(def lazy-schedule (->> (parsed-input 1)
                        (map id->sched)))

(defn answer [x]
  (let [[idx s] x]
    (some #(if (> % 0) (* idx %) false) s)))

;; part 1
(->> (apply (partial map (fn [& args] (vec args))) lazy-schedule)
     (drop (dec arrival-time))
     (map-indexed vector)
     (some answer))

;; part 2 chinese remainder theorem

;; Source of extended-gcd and chinese_remainder:
;; https://rosettacode.org/wiki/Chinese_remainder_theorem#Clojure
 
(defn extended-gcd
  "The extended Euclidean algorithm--using Clojure code from RosettaCode for Extended Eucliean
  (see http://en.wikipedia.orwiki/Extended_Euclidean_algorithm)
  Returns a list containing the GCD and the Bézout coefficients
  corresponding to the inputs with the result: gcd followed by bezout coefficients "
  [a b]
  (cond (zero? a) [(math/abs b) 0 1]
        (zero? b) [(math/abs a) 1 0]
        :else (loop [s 0
                     s0 1
                     t 1
                     t0 0
                     r (math/abs b)
                     r0 (math/abs a)]
                (if (zero? r)
                  [r0 s0 t0]
                  (let [q (quot r0 r)]
                    (recur (- s0 (* q s)) s
                           (- t0 (* q t)) t
                           (- r0 (* q r)) r))))))

(defn chinese_remainder
  " Main routine to return the chinese remainder "
  [n a]
  (let [prod (apply * n)
        reducer (fn [sum [n_i a_i]]
                  (let [p (quot prod n_i)           ; p = prod / n_i
                        egcd (extended-gcd p n_i)   ; Extended gcd
                        inv_p (second egcd)]        ; Second item is the inverse
                    (+ sum (* a_i inv_p p))))
        sum-prod (reduce reducer 0 (map vector n a))] ; Replaces the Python for loop to sum
                                                      ; (map vector n a) is same as
        ;                                             ; Python's version Zip (n, a)
    (mod sum-prod prod)))                             ; Result line

(defn parse-part-2 [lines]
  (let [[_ schedule] lines]
    (->> (str/split schedule #",")
         (map #(if (= % "x") "0" %))
         (map read-string)
         (map-indexed vector)
         (filter #(not= 0 (% 1))))))

(def contest-seq (parse-part-2 input))
(def N (reduce #(* %1 (%2 1)) 1 contest-seq))

(->> (chinese_remainder (vec (map #(% 1) contest-seq)) 
                        (vec (map #(% 0) contest-seq)))
     (mod N))
