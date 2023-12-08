(ns day3
  (:require [clojure.string :as str])
  (:require [instaparse.core :as insta]))

(defn lines [filepath]
  (str/split-lines (slurp filepath)))

;; 467..114..
;; ...*......
;; ..35..633.
;; ......#...
;; 617*......
;; .....+.58.
;; ..592.....
;; ......755.
;; ...$.*....
;; .664.598..

(def serial-numbers
  (insta/parser
   "I = (D|W|S)*
    D = #'[0-9]+'
    S = #'[*#+@$*&%/=-]'
    W = '.'"))

(serial-numbers ".@./...+.58.")

(defn remove-whitespace [tokens]
  (filter #(not= :W (first %)) tokens))

(defn filter-nested [tokens symbol]
  (filter #(= symbol (first %)) tokens))

(defn parse-line [parser number line]
  (let [parsed (parser line)
        spans (map #(cons number (insta/span %)) parsed)]
    (remove-whitespace (map merge (rest parsed) (rest spans)))))

(defn parse-serials-line [number line]
  (parse-line serial-numbers number line))

(defn clean-symbol [[_ _ [line start _]]]
  [line start])

(defn clean-digit [[_ digit-str coords]]
  [(parse-long digit-str) coords])

(defn has-overlap? [list1 list2]
  (some #(contains? (set list2) %) list1))

(has-overlap?
 [[-1 -1] [-1 0] [-1 1] [-1 2] [-1 3] [0 -1] [0 4] [1 -1] [1 0] [1 1] [1 2] [1 3]]
 [[1 3] [3 6] [4 3] [5 5] [8 3] [8 5]])

(defn edge-coords [[line start & [end-coord]]] ; -> ([line position]…)
  (let [end (or end-coord (+ start 1))
        top-bottom-range (range (dec start) (inc end))
        top-coords (map #(identity [(dec line) %]) top-bottom-range)
        bottom-coords (map #(identity [(inc line) %]) top-bottom-range)
        left (vector [line (dec start)])
        right (vector [line end])]
    (concat top-coords left right bottom-coords)))

;; 467..114..
;; ...*......
;; ..35..633.
;; ......#...
;; 617*......
;; .....+.58.
;; ..592.....
;; ......755.
;; ...$.*....
;; .664.598..

;; 0|012345
;; 1|012345
;; 2|012345
;; 3|012345
;; 4|012*45
;; 5|012345

(edge-coords [4 0 3])
(edge-coords [4 3])

(apply concat (map-indexed parse-serials-line (lines "data/3-test.txt")))

(defn part1 [filename]
  (let [data (apply concat (map-indexed parse-serials-line (lines filename)))
        digits (map clean-digit (filter-nested data :D))
        symbol-coords (map clean-symbol (filter-nested data :S))
        serial-numbers (filter (fn [[_ coords]] (has-overlap? (edge-coords coords) symbol-coords)) digits)
        raw-serials (map first serial-numbers)]
    (reduce + raw-serials)))


(has-overlap? (edge-coords [7 6 9]) [[1 3] [3 6] [4 3] [5 5] [8 3] [8 5]])

(part1 "data/3-test.txt"); => 4361
(part1 "data/3-data.txt"); => 521515


(def gear-ratios
  (insta/parser
   "I = (D|W|S)*
    D = #'[0-9]+'
    W = #'[*#+@$&%/=.-]'
    S = '*'"))

(gear-ratios "617*..#..%..")

(defn parse-gears-line [number line]
  (parse-line gear-ratios number line))

(apply concat (map-indexed parse-gears-line (lines "data/3-test.txt")))

(defn has-start-or-end [[digit-line digit-start digit-end] gear-coord]
  (or (= gear-coord [digit-line digit-start]) (= gear-coord [digit-line (dec digit-end)])))

(has-start-or-end [1 2 3] [1 2])
(has-start-or-end [1 2 3] [1 3])
(has-start-or-end [1 2 3] [2 3])
(has-start-or-end [0 0 3] [0 3])

(defn contains-serial? [edge-coords [_ serial-coords]]
  (some #(has-start-or-end serial-coords %) edge-coords))

(contains-serial? [[0 1] [0 2] [0 3]] [58 [0 0 3]])

(defn touching-digits [edge-coords serials]
  (filter #(contains-serial? edge-coords %) serials))


(defn part2 [filename]
  (let [data (apply concat (map-indexed parse-gears-line (lines filename)))
        digits (map clean-digit (filter-nested data :D))
        gear-coords (map clean-symbol (filter-nested data :S))
        gear-edges (map edge-coords gear-coords)
        gear-adjacent-serials (map #(touching-digits % digits) gear-edges)
        pairs (filter #(= 2 (count %)) gear-adjacent-serials)
        products (map (fn [[[lhs _] [rhs _]]] (* lhs rhs)) pairs)]
    (reduce + products)))

(part2 "data/3-test.txt") ; => 467835
(part2 "data/3-data.txt") ; => 69527306