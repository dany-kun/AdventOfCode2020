(ns day7.core
  (:use [clojure.test]))

(defn keyword- [name]
  (keyword (clojure.string/replace name " " "-")))

(defn- parse-input [file]
  (let [input (slurp (str "resources/" file))]
    (loop [[line & lines] (clojure.string/split-lines input)
           rules {}]
      (if (not lines)
        rules
        (let [[_ bag r] (re-matches #"(.*) bags contain (.*)." line)
              bag-rules (cond
                          (= "no other bags" r) {}
                          :else (->> (clojure.string/split r #", ")
                                     (map #(re-matches #"([0-9]+) (.*) bag.*" %))
                                     (map #(let [[_ count bag] %] {(keyword- bag) (clojure.edn/read-string count)}))
                                     (into {})))]
          (recur lines (conj rules {(keyword- bag) bag-rules})))))))

(defn- includes? [bag [key _] graph]
  (let [vs (graph key)]
    (cond
      (empty? vs) false
      (some (fn [[k _]] (= k bag)) vs) true
      :else (reduce (fn [res next] (or res (includes? bag next graph))) false vs))))

(defn- aggregate-a [bag graph]
  (count (filter #(includes? bag % graph) graph)))

(defn- sum-up [[k c] graph count]
  (+ count (* c (inc (apply + (map #(sum-up % graph 0) (graph k)))))))

(defn- aggregate-b [bag count graph]
  (dec (sum-up bag graph count)))

(defn day7 [resource agg]
  (->> (parse-input resource)
       (agg)))

(deftest sample-a []
                  (is (= 4
                         (day7 "day7.sample.1.txt" #(aggregate-a :shiny-gold %)))))

(deftest sample-b []
                  (is (= 32
                         (day7 "day7.sample.1.txt" #(aggregate-b [:shiny-gold 1] 0 %)))))

(run-tests 'day7.core)

