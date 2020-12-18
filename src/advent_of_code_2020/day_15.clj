(ns advent-of-code-2020.day-15)

(def s {:t 6 
        :x 6 
        :num->turns {2 '(1) 1 '(2) 10 '(3) 11 '(4) 0 '(5) 6 '(6)}})

(defn record-turn [s x]
  (let [t (inc (s :t))]
    (-> s 
      (update-in [:num->turns x] #(conj % t))
      (assoc :x x)
      (assoc :t t))))

(defn repeated? [c]
  (> (count c) 1))

(defn turn [s]
  (let [x-turns (get-in s [:num->turns (s :x)])]
    (record-turn s (if (repeated? x-turns) 
                     (apply - (take 2 x-turns))  
                     0))))

;; part 1
(->> (iterate turn s)
     (drop (- 2020 (s :t)))
     first
     :x)

;; part 2
(->> (iterate turn s)     
     (drop (- 30000000 (s :t)))
     first
     :x)