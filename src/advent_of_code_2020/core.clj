(ns advent-of-code-2020.core
  (:require [clojure.string :as str]))

(defn read-lines [path]
  (map read-string (-> (slurp path)
                       (str/split #"\r\n"))))