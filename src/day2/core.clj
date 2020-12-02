(ns day2.core)

(defn- password [str]
  (->> str
       (re-matches #"([0-9]*)-([0-9]*) ([a-z]): (.*)")))

(defn- valid-a? [[_ min max char pwd]]
  (let [min (clojure.edn/read-string min)
        max (clojure.edn/read-string max)
        count (- (count pwd) (count (clojure.string/replace pwd char "")))]
    (and (>= count min) (<= count max))))

(defn- valid-b? [[_ first second char pwd]]
  (let [get-at #(str (nth pwd (-  (clojure.edn/read-string %) 1) nil))
        first (get-at first)
        second (get-at second)]
    (and (not= first second) (or (= first char) (= second char)) )))

(defn- read-input []
  (->> (slurp "resources/day2.txt")
       clojure.string/split-lines
       (map password)))

(defn day2 [predicate]
  (->> (read-input)
       (filter predicate)
       count))