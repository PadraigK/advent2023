(ns day6
  (:require [clojure.string :as str]
            [clojure.test :refer :all]))


(defn lines [filepath]
  (str/split-lines (slurp filepath)))

(defn extract-numbers [string]
  (map parse-long (re-seq #"\d+" string)))

(map extract-numbers (lines "data/6-test.txt"))

(defn count-ways-to-win [duration record]
  ())

(deftest test-count-ways-to-win
  (is (= 4 (count-ways-to-win 7 9)))
  (is (= 8 (count-ways-to-win 15 40)))
  (is (= 9 (count-ways-to-win 30 200))))

(comment
  (print 3))
  

