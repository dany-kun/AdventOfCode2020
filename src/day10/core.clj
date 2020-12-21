(ns day10.core
  (:use [clojure.test]))

(defn- parse-input [resource]
  (let [input (slurp (str "resources/" resource))]
    (->> input
         (clojure.string/split-lines)
         (map clojure.edn/read-string))))



(defn- count-diff [list]
  ; Reduce list of sorted entries grouping those by steps between two entries
  ; (1 3 4 7 10 11 12 13) -> ((1) (2) (1) (3 3) (1 1 1))
  (loop [prev 0
         [head & rest] list
         [last-steps & others :as steps] '()]
    (let [add-step (fn [step]
                     (cond
                       (some #{step} last-steps) (conj others (conj last-steps step))
                       :else (conj steps (into '() [step]))))]
      (if head
        (let [step (- head prev)]
          (recur head rest (add-step step)))
        (add-step 3)))))


(defn- aggr-a [groups]
  (let [diffs (group-by identity (flatten groups))]
    (* (count (diffs 1)) (count (diffs 3)))))

; [1] -> (1) 1 option
; [1 1] -> (1 1) (2) 2 options
; [1 1 1] -> (3) (2 1) (1 2) (1 1 1) 4 options
; [1 1 1 1] -> (3 1) (2 1 1) (2 2) (1 3) (1 2 1) (1 1 2) (1 1 1 1) -> 7
; n * (n- 1) / 2 + 1 ?? (purey empirical guess but seems to work; no idea why
(defn- aggr-b [groups]
  (->> groups
       (filter #(some #{1} %))
       (map count)
       (map #(* % (dec %)))
       (map #(/ % 2))
       (map inc)
       (apply *)))

(defn day10 [resource agg]
  (->> (parse-input resource)
       (sort)
       (count-diff)
       (agg)))


(deftest sample-a []
                  (is (= 35
                         (day10 "day10.sample.1.txt" aggr-a))))

(deftest sample-b []
                  (is (= 220
                         (day10 "day10.sample.2.txt" aggr-a))))

(deftest sample-a-2 []
                    (is (= 8
                           (day10 "day10.sample.1.txt" aggr-b))))

(deftest sample-b-2 []
                    (is (= 19208
                           (day10 "day10.sample.2.txt" aggr-b))))


(run-tests 'day10.core)

