(ns advent-of-code-2020.day-07
  (:require [clojure.string :as str]))

(def input (-> (slurp "resources/day_07/input")
               (str/replace #"\b(bag|bags)\b" "")
               (str/replace #"\." "")
               (str/split #"\r\n")))

(defn str->keyword [x]
  (keyword (str/replace x #" " "-")))

(defn parse-node [x]
  (let [[n b] (str/split x #" " 2)]
    {(str->keyword b) (read-string n)}))

(defn parse-edges [x]
  (if (= "no other"(first x))
    nil
    (into {} (map parse-node x))))

(defn parse-rule [x]
  (let [[h & t] x]
    [(str->keyword h) (parse-edges t)]))

(def graph (->> input
            (map #(str/split % #"contain|,"))
            (map #(map str/trim %))
            (map parse-rule)
            (into {})))

;; graph represented as an adjacency map with weights, se ex below:
(def ex {:shiny-gold   {:dotted-black 3 :vibrant-aqua 1}
         :dotted-black {:faded-beige 6}
         :faded-beige  {}
         :vibrant-aqua {}})

;; part 1
(defn add-to-node [node edge]
  (fn [acc]
    (conj (if (nil? acc) {} acc) {node edge})))

(defn reverse-node [g' node edges]
  (reduce #(update-in %1 [(%2 0)] (add-to-node node (%2 1))) g' edges))

(defn invert [g]
  (reduce-kv reverse-node
             (zipmap (keys g) (map (constantly {}) (vals g)))
             g))

(def inverted-graph (invert graph))

(defn dfs
  [g src]
  (loop [stack   (vector src) 
         visited []]        
    (if (empty? stack)     
      visited
      (let [curr        (peek stack)
            neighbors   (set (keys (g curr)))
            not-visited (apply (partial disj neighbors) visited)
            stack'      (into (pop stack) not-visited)]
        (recur stack' (conj visited curr))))))

(->> (dfs inverted-graph :shiny-gold)
     ((comp dec count set)))

;; part 2
(defn count-rec
  [g v]
  (let [neighbors (g v)]    
    (if (empty? neighbors) 
      1
      (->> neighbors
           (map #(* (% 1) (count-rec g (% 0))))
           (apply +)
           (+ 1))
    )))

(dec (count-rec graph :shiny-gold))
