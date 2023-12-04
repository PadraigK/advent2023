(ns day2
  (:require [clojure.string :as str]))

(defn games [filepath]
  (str/split-lines (slurp filepath)))

(defn rounds [game-string]
  (str/split game-string #":"))

(map rounds (games "data/2-test.txt"))

(defn parse-block-color [block-string]
  [(cond
     (str/ends-with? block-string "blue") :blue
     (str/ends-with? block-string "red") :red
     (str/ends-with? block-string "green") :green)
   (parse-long (re-find #"\d+" block-string))])

(defn round-from-string [round-string]
  (let [block-strings (map #(str/split % #",") round-string)
        round (map #(into {} (map parse-block-color %)) block-strings)]
    round))

(defn gameid-from-string [s]
  (parse-long (subs s 5)))

(defn exceeds-limits [round]
  (cond
    (> (:blue round 0) 14) true ; 14 limit set in the puzzle
    (> (:red round 0) 12) true ; 12 limit set in the puzzle
    (> (:green round 0) 13) true ; 13 limit set in the puzzle
    :else false))

(defn game-possible? [rounds]
  (not (some true? (map exceeds-limits rounds))))

(defn parse-game-str [game-str]
  (let [[game-title rounds-str] (str/split game-str #":")
        game-rounds (round-from-string (str/split rounds-str #";"))]
    [(gameid-from-string game-title) game-rounds]))

(defn game-ids-of-possible-games [[game-id game-rounds]]
  (if (game-possible? game-rounds) game-id nil))

(defn parsed-games [filename]
  (map parse-game-str (games filename)))

(defn part1 [filename]
  (apply + (remove nil? (map game-ids-of-possible-games (parsed-games filename)))))

(part1 "data/2-test.txt"); 8
(part1 "data/2-data.txt"); 2278

;; part 2

(defn max-count-per-block [rounds]
  (let [pivot (apply
               merge-with conj
               {:red [] :green [] :blue []}
               rounds)]
    (map (fn [[key items]] {key (apply max items)}) pivot)))



(map
 (fn [[_ rounds]] (vals (max-count-per-block rounds)))
 (parsed-games "data/2-test.txt"))

(parsed-games "data/2-test.txt")

(defn part2 [filename]
  (let [maxes-per-game (map
                        (fn [[_ rounds]] (max-count-per-block rounds))
                        (parsed-games filename))]
    (conj maxes-per-game)))

(part2 "data/2-test.txt")

(conj {:red 4} {:green 2} {:blue 6})