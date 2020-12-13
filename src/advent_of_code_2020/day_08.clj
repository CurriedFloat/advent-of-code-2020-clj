(ns advent-of-code-2020.day-08
  (:require [clojure.string :as str]))

(def input (-> (slurp "resources/day_08/input")
               (str/split #"\r\n")))

(defn parse-op [x]
  (let [[op num] (str/split x #" ")]
    [(keyword op) (read-string num)]))

(def instructions (->> input
                       (map parse-op)
                       (map-indexed (fn [idx itm] (conj itm idx)))
                       (into [])))

(def init {:i 0 :acc 0 :log []})

(defn step [s] 
  (update s :i inc))

(defn record [s] 
  (update s :log #(conj % (s :i))))

(defn inf-loop-detected? [log]
  (not (apply distinct? log)))

(defn terminated? [instructions x] 
  (= x (count instructions)))

(defn handle-op [x s]
  (let [[op num] x]
   (cond
    (= op :jmp) (-> s (update :i #(+ (dec num) %)))
    (= op :acc) (-> s (update :acc #(+ % num)))
    :else s)))

(defn run [instructions init]
  (loop [s init]
    (let [cur (s :i)
          op (get instructions cur)]
      (if (or (terminated? instructions cur) 
              (inf-loop-detected? (conj (s :log) cur)))
        s
        (recur (-> (handle-op op s)
                   record
                   step))))))

;; part 1
((run instructions init) :acc)

;; part 2
(defn switch-op [x]
  (update x 0 #(cond
                 (= % :jmp) :nop
                 (= % :nop) :jmp
                 :else %)))

(defn replace-op [instructions op]
  (assoc instructions (op 2) op))

(def after-last (count instructions))

(->> instructions
     (filter #(not= :acc (% 0)))
     (map switch-op)
     (map (partial replace-op instructions))
     (map #(run % init))
     (some #(when (= after-last (% :i)) (% :acc))))
