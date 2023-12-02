(ns day1
  (:require [clojure.string :as str]))

(defn char-seq [s]
  (map str (vec s)))

(defn dirty-values [filepath]
  (str/split-lines (slurp filepath)))

(defn digits [s]
  (remove nil? (map parse-long (char-seq s))))

(defn extract-calibration [dirty-val]
  (let [d (digits dirty-val)]
    (apply str [(first d) (last d)])))

(defn part1 [filepath]
  (apply + (map parse-long (map extract-calibration (dirty-values filepath)))))

(part1 "data/1-test.txt")
(part1 "data/1-data.txt")

;; on to part2 
(def replacements [["1" "one"]
                   ["2" "two"]
                   ["3" "three"]
                   ["4" "four"]
                   ["5" "five"]
                   ["6" "six"]
                   ["7" "seven"]
                   ["8" "eight"]
                   ["9" "nine"]])

(defn numerify [s]
  (reduce (fn [acc [digit text]] (str/replace acc text digit))
          s
          replacements))

(numerify "eightwothree")

(map numerify (dirty-values "data/1-test2.txt"))

(defn part2 [filepath]
  (apply + (map parse-long (map extract-calibration (map numerify (dirty-values filepath))))))

(part2 "data/1-test2.txt")
(part1 "data/1-data.txt")

;; nope, got owned by the `eightwothree -> eigh23` gotcha, lets try again...

(def numbers [["1" 1]
              ["one" 1]
              ["2" 2]
              ["two" 2]
              ["3" 3]
              ["three" 3]
              ["4" 4]
              ["four" 4]
              ["5" 5]
              ["five" 5]
              ["6" 6]
              ["six" 6]
              ["7" 7]
              ["seven" 7]
              ["8" 8]
              ["eight" 8]
              ["9" 9]
              ["nine" 9]])

(defn positions-and-digits [dirty-val]
  (remove #(some nil? %)
          (concat
           (map (fn [[string digit]] [(str/index-of dirty-val string) digit]) numbers)
           (map (fn [[string digit]] [(str/last-index-of dirty-val string) digit]) numbers))))

(defn extract-calibration [dirty-val]
  (let [items (sort (positions-and-digits dirty-val))
        [_ first-val] (first items)
        [_ last-val] (last items)]
    (apply str [first-val last-val])))

(defn part2 [filepath]
  (apply + (map parse-long (map extract-calibration (dirty-values filepath)))))

(part2 "data/1-test2.txt")
(part2 "data/1-data.txt")