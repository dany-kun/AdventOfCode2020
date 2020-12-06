(ns day6.core
  (:use [clojure.test]))

(defn- parse-input [file]
  (let [input (slurp (str "resources/" file))]
    (loop [[line & r] (clojure.string/split-lines input)
           [g & gs :as groups] []]
      (cond
        (not line) groups
        (clojure.string/blank? line) (recur r (conj groups []))
        :else (recur r (conj gs (conj g (set line))))))))

(def aggregate-1
  (comp count #(apply clojure.set/union %)))

(def aggregate-2
  (comp count #(apply clojure.set/intersection %)))

(defn day6 [resource agg]
  (->> (parse-input resource)
       (map agg)
       (reduce +)))

(deftest sample-a []
                  (is (= 11
                         (day6 "day6.sample.1.txt" aggregate-1))))

(deftest sample-b []
                  (is (= 6
                         (day6 "day6.sample.1.txt" aggregate-2))))

(run-tests 'day6.core)

