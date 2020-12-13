(ns day8.core
  (:use [clojure.test]))


(defn- parse-input [resource]
  (let [input (slurp (str "resources/" resource))]
    (clojure.string/split-lines input)))

(defn- operate [operator]
  (let [[op v] (clojure.string/split operator #" ")]
    (fn [[cursor aggregator]]
      (case op
        "nop" [(inc cursor) aggregator]
        "acc" [(inc cursor) (+ aggregator (clojure.edn/read-string v))]
        "jmp" [(+ cursor (clojure.edn/read-string v)) aggregator]))))

(defn- aggregate-a [operations]
  (loop [[cursor agg :as p] [0 0] idx #{}]
    (if (contains? idx cursor)
      agg
      (recur ((operate (operations cursor)) p) (conj idx cursor)))))

(defn- aggregate-b [operations]
  (loop [[cursor agg :as p] [0 0] idx #{}]
    (when-not (contains? idx cursor)
      (when-let [operator (nth operations cursor nil)]
        (let [[nc na] ((operate operator) p)]
          (if (and (= (inc cursor) nc) (= nc (count operations)))
            na
            (recur [nc na] (conj idx cursor))))))))

(defn- switch-at [i ops]
  (map-indexed #(if (= %1 i)
                  (if (clojure.string/includes? %2 "nop")
                    (clojure.string/replace %2 "nop" "jmp")
                    (clojure.string/replace %2 "jmp" "nop"))
                  %2)
               ops))

(defn- combine [ops]
  (map #(switch-at % ops) (range (count ops))))

(defn- find-b [resource]
  (first (filter identity (map aggregate-b (combine resource)))))

(defn day8 [resource agg]
  (->> (parse-input resource)
       (agg)))

(deftest sample-a []
                  (is (= 5
                         (day8 "day8.sample.1.txt" aggregate-a))))

(deftest sample-b []
                  (is (= 8
                         (day8 "day8.sample.1.txt" find-b))))

(run-tests 'day8.core)

