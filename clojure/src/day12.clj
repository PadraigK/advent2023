(ns day12
  (:require [clojure.string :as str]))

(defn lines [filepath]
  (str/split-lines (slurp filepath)))

(lines "data/12-test.txt")

(map #(str/split % #"\s") (lines "data/12-test.txt"))

(def template (partition-by identity "???.### 1,1,3 "))
(str/replace-first template "\?+")

(reduce (fn [acc item] (str/replace-first item "\?+")))