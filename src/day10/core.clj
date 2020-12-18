(ns day10.core
  (:use [clojure.test]))

(defn- parse-input [resource]
  (let [input (slurp (str "resources/" resource))]
    (->> input
         (clojure.string/split-lines)
         (map clojure.edn/read-string))))



(defn- count-diff [list]
  (loop [prev 0
         [head & rest] list
         [one three] [0 0]]
    (if head
      (recur head rest (case (- head prev)
                         1 [(inc one) three]
                         3 [one (inc three)]
                         [one three]))
      [one (inc three)])))


(defn day10 [resource]
  (->> (parse-input resource)
       (sort)
       (count-diff)
       (apply *)))


(deftest sample-a []
                  (is (= 35
                         (day10 "day10.sample.1.txt"))))

(deftest sample-b []
                  (is (= 220
                         (day10 "day10.sample.2.txt"))))



(run-tests 'day10.core)

