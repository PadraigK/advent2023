(ns day4
  (:require [clojure.math :refer [pow]]
            clojure.set
            [clojure.string :as str]))


;; Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
;; Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
;; Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
;; Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
;; Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
;; Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11

(defn lines [filepath]
  (str/split-lines (slurp filepath)))

(defn extract-numbers [string]
  (map parse-long (re-seq #"\d+" string)))

(defn parse-data [[card-id played-numbers winning-numbers]]
  (let [id (subs card-id 5)
        played (set (extract-numbers played-numbers))
        winning (set (extract-numbers winning-numbers))]
    {:id id :played played :winning winning}))

(defn part1 [filename]
  (let [string-data (map #(str/split % #":|\|") (lines filename))
        parsed-cards (map parse-data string-data)
        winning-nums (map #(clojure.set/intersection
                            (% :played)
                            (% :winning)) parsed-cards)
        count-winning (remove zero? (map count winning-nums))]
    (reduce + (map #(pow 2 (- % 1)) count-winning))))

(part1 "data/4-test.txt"); => 13 
(part1 "data/4-data.txt"); => 24848.0


(defn calc [items amount]
  (reduce + 1 (take amount (reverse items))))

(calc [1 2 3] 0)
(calc [1 2 3] 1)
(calc [1 2 3] 2)

(defn part2 [filename]
  (let [string-data (map #(str/split % #":|\|") (lines filename))
        parsed-cards (map parse-data string-data)
        winning-nums (map #(clojure.set/intersection
                            (% :played)
                            (% :winning)) parsed-cards)
        count-winning (map count winning-nums)
        values (reduce (fn [acc val] (conj acc (calc acc val))) [] (reverse count-winning))]
    (reduce + values)))

(part2 "data/4-test.txt"); => 30
(part2 "data/4-data.txt"); => 7258152
