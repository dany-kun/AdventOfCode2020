(ns day11.core
  (:use [clojure.test]))

(defn- parse-input [resource]
  (let [input (slurp (str "resources/" resource))]
    (->> input
         (clojure.string/split-lines))))

(defn- adjacent-seats [x y map]
  (filter identity (for [step-x [-1 0 1]
                         step-y [-1 0 1]
                         :when (not (= 0 step-x step-y))]
                     (when-let [col (nth map (+ y step-y) nil)]
                       (nth col (+ x step-x) nil)))))

(defn- adjacent-seats-b [x y map]
  (filter identity (for [step-x [-1 0 1]
                         step-y [-1 0 1]
                         :when (not (= 0 step-x step-y))]
                     (loop [[xs ys] [x y]]
                       (let [new-y (+ ys step-y)
                             new-x (+ xs step-x)]
                         (when-let [col (nth map new-y nil)]
                           (when-let [p (nth col new-x nil)]
                             (if (= \. p)
                               (recur [new-x new-y])
                               p))))))))

(defn- update-seat [x y map [find-adjacent n]]
  (let [seat-value (when-let [col (nth map y nil)] (nth col x nil))
        adjacent-seats (find-adjacent x y map)
        occupied-adjacent-seats (count (filter #{\#} adjacent-seats))]
    (cond
      (and (= seat-value \L) (= 0 occupied-adjacent-seats)) \#
      (and (= seat-value \#) (<= n occupied-adjacent-seats)) \L
      :else seat-value)))

(defn- print-map [m]
  (println "---------------------------")
  (println (clojure.string/join \newline (map clojure.string/join m))))

(defn- simulate [agg input]
  (let [rows (count input)
        cols (count (first input))]
    (iterate (fn [[prev curr]] [curr (for [y (range rows)]
                                       (for [x (range cols)]
                                         (update-seat x y curr agg)))])
             [nil input])))



(defn day11 [resource agg]
  (->> (parse-input resource)
       (simulate agg)
       (take-while (fn [[prev curr]] (not= prev curr)))
       (last)
       (last)
       (flatten)
       (filter #{\#})
       (count)))


(deftest sample-a []
                  (is (= 37
                         (day11 "day11.sample.1.txt" [adjacent-seats 4]))))

(deftest sample-b []
                  (is (= 26
                         (day11 "day11.sample.1.txt" [adjacent-seats-b 5]))))



(run-tests 'day11.core)

