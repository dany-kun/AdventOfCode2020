(ns day3.core
  (:use [clojure.test]))

(defn count-tree [[x y] area]
  (let [row (get area y)]
    (case (get row (mod x (count row)))
      \# 1
      0)))

(defn- count-trees [slope [_ y :as point] count area]
  (if (not (get area y))
    count
    (count-trees slope
                 (mapv + slope point)
                 (+ count (count-tree point area))
                 area)))

(defn- read-input [resource]
  (->> (slurp (str "resources/" resource))
       clojure.string/split-lines))

(defn- count-trees-slopes [slopes area]
  (reduce * (map #(count-trees % [0 0] 0 area) slopes)))

(def slopes-a [[3 1]])
(def slopes-b [[1 1] [3 1] [5 1] [7 1] [1 2]])

(defn day3 [resource slopes]
  (->> (read-input resource)
       (count-trees-slopes slopes)))

(deftest sample-a []
                  (is (= 7
                         (day3 "day3.sample1.txt" slopes-a))))

(deftest sample-b []
                  (is (= 336
                         (day3 "day3.sample1.txt" slopes-b))))

(run-tests 'day3.core)

