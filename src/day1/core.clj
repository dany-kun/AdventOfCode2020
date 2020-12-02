(ns day1.core)

(defn- read-input []
  (->> (slurp "resources/day1.txt")
       clojure.string/split-lines
       (map clojure.edn/read-string)))

(defn find-components [total components-count]
  (let [inputs (read-input)]
    (first (filter #(= total (reduce + %))
                   (case components-count
                     2 (for [x inputs y inputs] [x y])
                     3 (for [x inputs y inputs z inputs] [x y z])
                     (throw (Exception. (str "Unhandled count" components-count))))))))


(defn day1 [components-count]
  (let [components (find-components 2020 components-count)]
    (reduce * components)))