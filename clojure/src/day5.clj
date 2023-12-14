(ns day5
  (:require [clojure.string :as str]
            [clojure.test :refer :all]
            [criterium.core :refer [quick-bench]]))

;; seeds: 79 14 55 13

;; seed-to-soil map:
;; 50 98 2
;; 52 50 48

;; soil-to-fertilizer map:
;; 0 15 37
;; 37 52 2
;; 39 0 15

(defn lines [filepath]
  (str/split-lines (slurp filepath)))

(def test-data (lines "data/5-test.txt"))

(defn extract-numbers [string]
  (map parse-long (re-seq #"\d+" string)))

(defn translation-ranges [translation-map]
  (sort-by :start (map
                   (fn [[dest src len]] {:start src :end (+ src len) :delta (- dest src)})
                   translation-map)))

(translation-ranges [[50 98 2]])
(translation-ranges [[52 50 48] [1 1 1]])

(defn in-range? [start end x]
  (and (>= x start) (< x end)))

(in-range? 50 52 52)

(defn matching-translation [seed-number translations]
  (first (filter (fn [item] (in-range? (:start item) (:end item) seed-number)) translations)))

(:delta (matching-translation 52 [{:start 50, :end 52, :delta 48} {:start 52, :end 100, :delta -2}]))

(defn apply-translation [seed-number translations]
  (let [translation (matching-translation seed-number translations)
        delta (or (:delta translation) 0)]
    (+ seed-number delta)))

(apply-translation 10 [{:start 50, :end 52, :delta 48} {:start 52, :end 100, :delta -2}])

(defn apply-all-translations [seed-number all-translations]
  (reduce
   (fn [acc translations] (apply-translation acc translations))
   seed-number
   all-translations))

(def test-trans [[{:start 50, :end 52, :delta 48} {:start 52, :end 100, :delta -2}]
                ;;  [{:start 0, :end 37, :delta 15} {:start 37, :end 39, :delta 15} {:start 39, :end 54, :delta -39}]
                ;;  [{:start 49, :end 57, :delta 4}
                ;;   {:start 0, :end 42, :delta 11}
                ;;   {:start 42, :end 49, :delta -42}
                ;;   {:start 57, :end 61, :delta -50}]
                ;;  [{:start 88, :end 95, :delta -70} {:start 18, :end 88, :delta 7}]
                ;;  [{:start 45, :end 68, :delta 32} {:start 81, :end 100, :delta -36} {:start 68, :end 81, :delta -4}]
                ;;  [{:start 0, :end 1, :delta 69} {:start 1, :end 70, :delta -1}]
                ;;  [{:start 60, :end 97, :delta -4} {:start 56, :end 60, :delta 37}]
                 ])

(apply-all-translations 79 test-trans)

(defn part1 [filename]
  (let [input-lines (lines filename)
        seeds (extract-numbers (first input-lines))
        translation-strings (map rest (remove #(some empty? %) (rest (partition-by empty? input-lines))))
        translation-maps (map #(map extract-numbers %) translation-strings)
        all-translations (vec (map translation-ranges translation-maps))
        locations (pmap #(apply-all-translations %1 all-translations) seeds)]
    (apply min locations)))

(part1 "data/5-test.txt")
(part1 "data/5-data.txt")

(partition 2 [12 42 5 3 5 3 3])

(defn range-vec [[start len]]
  (range start (+ start len)))

(range-vec [2 5])

(defn seeds [numbers]
  (apply concat (map range-vec (partition 2 numbers))))


;;; the plan for part 2

;; take each seed range [0 10]   [0 4] +2, [4 10] +1
;;  take each list of mapping dictionaries {:start 0, :end 4, :delta 2}, {:start 4, :end 12, :delta 1}

;;  -> find the sub-ranges in the mapping dictionary (where s.start > m.start and s.start < m.end)
;;  -> break the seed range into multiple seed ranges and apply the delta to the start+end positions
;;  [0 4] -> [2 6], [4 10] -> [5 11]
;;  [0 10 :seed]  -> [2 6 :soil] [5 11 :soil]

(defn overlap [[t-start t-end] [r-start r-end]]
  (let [?start (cond
                 (and (< r-start t-end) (> r-end t-start)) (max t-start r-start)
                 :else nil)
        ?end (cond
               (and (> r-end t-start) (<= t-start r-end)) (min t-end r-end)
               :else nil)
        result [?start ?end]]
    (if (every? identity result) result nil)))

(defn increment-by [delta coll] (map #(+ delta %) coll))

(defn apply-range-translations [range translations]
  (remove empty? (map
                  #(increment-by (:delta %) (overlap [(:start %) (:end %)] range))
                  translations)))

(apply-range-translations [0 15] [{:start 1, :end 4, :delta 2}, {:start 4, :end 12, :delta 1}, {:start 40, :end 120, :delta 1}])
(apply-range-translations [3 20] [{:start 1, :end 4, :delta 2}, {:start 4, :end 12, :delta 1}, {:start 40, :end 120, :delta 1}])

(defn apply-translations-to-range-group [ranges translations]
  (apply concat (map #(apply-range-translations % translations) ranges)))

(apply-translations-to-range-group
 [[0 15] [3 20]]
 [{:start 1, :end 4, :delta 2}, {:start 4, :end 12, :delta 1}, {:start 12, :end 120, :delta 1}])

(defn apply-all-translations-to-range-group [ranges all-translations]
  (reduce #(apply-translations-to-range-group %1 %2) ranges all-translations))

(defn seed-ranges [numbers]
  (map (fn [[start len]] [start (+ start len)]) (partition 2 numbers)))

(defn fill-empty [translations]
  (let [initial (:start (first translations))
        final (:end (last translations))
        initial-added (if (> initial 0) (cons {:start 0 :end initial :delta 0} translations) translations)
        final-added (if (< final Long/MAX_VALUE) (conj initial-added {:start final :end Long/MAX_VALUE :delta 0}) initial-added)]
    (sort-by :start final-added)))

(fill-empty [{:start 50, :end 98, :delta 2} {:start 98, :end 100, :delta -48}])

(defn part2 [filename]
  (let [input-lines (lines filename)
        seed-rs (seed-ranges (extract-numbers (first input-lines)))
        translation-strings (map rest (remove #(some empty? %) (rest (partition-by empty? input-lines))))
        translation-maps (map #(map extract-numbers %) translation-strings)
        all-translations (map translation-ranges translation-maps)
        fixed-translations (map fill-empty all-translations)
        locations (apply-all-translations-to-range-group seed-rs fixed-translations)
        lower-ends (map first locations)]
    (apply min lower-ends)))

(part2 "data/5-test.txt")
(part2 "data/5-data.txt") ;=> 28580589


((deftest apply-range-translations-test
   (testing "Ranges are split into sub-ranges along the translations dict and then moved according to the delta"
     (is (= [[2 6] [5 11]] (apply-range-translations [0 10] [{:start 0, :end 4, :delta 2}, {:start 4, :end 12, :delta 1}]))))))

((deftest overlap-test
   (testing "test overlap"
     (is (= [4 10] (overlap [4 12]    [0 10]))) ; right overlap
     (is (= [5 12] (overlap [2 12]    [5 20]))) ; left overlap
     (is (= [1 4]  (overlap [1 4]     [0 10]))) ;  inner overlap
     (is (= [5 20] (overlap [0 25]    [5 20]))) ;  outer overlap
     (is (nil?     (overlap [100 200] [0 10]))) ; no overlap (right)
     (is (nil?     (overlap [0 3]     [5 20])))) ; no overlap (left)
   ))

((deftest overlap-boundary-test
   (testing "test overlap"
     (is (= [4 10] (overlap [4 10]    [0 10]))) ; right overlap
     (is (= [5 12] (overlap [5 12]    [5 20]))) ; left overlap
     (is (= [0 10] (overlap [0 10]    [0 10]))) ; inner overlap
     (is (= [5 20] (overlap [5 20]    [5 20]))) ;  outer overlap
     (is (nil?     (overlap [100 200] [200 300]))) ; no overlap (right)
     (is (nil?     (overlap [0 3]     [3 20])))) ; no overlap (left)
   )) 

