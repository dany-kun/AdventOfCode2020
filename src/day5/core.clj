(ns day5.core
  (:use [clojure.test]))

(defn- parse-input [file]
  (let [input (slurp (str "resources/" file))]
    (clojure.string/split-lines input)))

(defn power-of-2 [n]
  (reduce * (repeat n 2)))

(defn find-position [pos pred]
  (->> pos
       (reverse)
       (map-indexed (fn [i x] (cond
                                (pred x) (power-of-2 i)
                                :else 0)))
       (reduce +)))

(defn- seat-id [pass]
  (let [[rows cols] (partition 7 7 "" pass)
        row (find-position rows #(= \B %))
        col (find-position cols #(= \R %))]
    (-> (* 8 row)
        (+ col))))


(defn- aggregate-a [ids] (apply max ids))
(defn- aggregate-b [ids]
  (loop [[x & xs] (sort ids)]
    (if (= 2 (- (first xs) x))
      (+ x 1)
      (recur xs))))

(defn day5 [resource agg]
  (->> (parse-input resource)
       (map seat-id)
       (agg)))

(deftest sample-a []
                  (is (= 567
                         (seat-id "BFFFBBFRRR"))))

(run-tests 'day5.core)

