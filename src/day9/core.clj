(ns day9.core
  (:use [clojure.test]))

(defn- parse-input [resource]
  (let [input (slurp (str "resources/" resource))]
    (->> input
         (clojure.string/split-lines)
         (map clojure.edn/read-string))))

(defn- sliding-window [preamble entries]
  (loop [queue []
         sums {}
         [head & rest] entries]
    (cond
      (nil? rest) (throw (Exception. "End of entries"))
      (or (< (count queue) preamble) (some #{head} (apply concat (vals sums))))
      (recur
        (into [] (take preamble (conj queue head)))
        ; Drop oldest item in the queue
        (let [popped-sums (filter (fn [[k v]] (< (count v) (dec preamble))) sums)]
          ; Add the sum to previous entries
          (-> (into {} (map (fn [[k v]] [k (conj v (+ k head))]) popped-sums))
              ; Add a new entry with an empty map
              (conj {head '()})))
        rest)
      :else head)))

(defn- find-consecutive [input target]
  (let [[range sum] (last (take-while (fn [[_ sum]] (<= sum target))
                                      (reductions
                                        (fn [[prev sum] i] [(conj prev i) (+ sum i)])
                                        ['() 0]
                                        input)))]
    (or (and (= sum target) range)
        (find-consecutive (drop 1 input) target))))

(defn day9 [resource preamble]
  (->> (parse-input resource)
       (sliding-window preamble)))

(defn day9-b [resource preamble]
  (let [input (parse-input resource)
        target (sliding-window preamble input)
        range (find-consecutive input target)]
    (+ (apply min range) (apply max range))))

(deftest sample-a []
                  (is (= 127
                         (day9 "day9.sample.1.txt" 5))))

(deftest sample-b []
                  (is (= 62
                         (day9-b "day9.sample.1.txt" 5))))


(run-tests 'day9.core)

