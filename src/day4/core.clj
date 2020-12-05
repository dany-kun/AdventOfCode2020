(ns day4.core
  (:use [clojure.test])
  (:require [clojure.spec.alpha :as s]))

; Parsing
(defn- to-kv [txt]
  (let [[k v] (clojure.string/split txt #":")]
    [(keyword "passport" k) v]))

(defn- to-card [entry]
  (into {} (map to-kv (clojure.string/split entry #" "))))

(defn- parse-input [file]
  (let [input (slurp (str "resources/" file))
        inlined-input (clojure.string/replace input \newline \space)]
    (->> (clojure.string/split inlined-input #"  ")
         (map to-card))))

; Predicates
(defn- to-int [x] (try (Integer/parseInt x) (catch Exception _ nil)))

(defn- height [regex min max]
  (comp
    #(s/int-in-range? min max %)
    to-int
    #(get % 1)
    #(re-matches regex %)))

(s/def :passport/byr (comp #(s/int-in-range? 1920 2003 %) to-int))
(s/def :passport/iyr (comp #(s/int-in-range? 2010 2021 %) to-int))
(s/def :passport/eyr (comp #(s/int-in-range? 2020 2031 %) to-int))
(s/def :passport/hgt (s/or :cm (height #"([0-9]*)cm" 150 194)
                           :in (height #"([0-9]*)in" 59 77)))
(s/def :passport/hcl #(re-matches #"#[0-9a-f]{6}" %))
(s/def :passport/ecl #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})
(s/def :passport/pid #(re-matches #"[0-9]{9}" %))
(s/def :passport/passport
  (s/keys :req [:passport/byr :passport/iyr :passport/eyr :passport/hgt
                :passport/hcl :passport/ecl :passport/pid]))

(defn- valid-a? [passport]
  (every? identity (map passport #{:passport/byr :passport/iyr
                                   :passport/eyr :passport/hgt
                                   :passport/hcl :passport/ecl
                                   :passport/pid})))

(defn- valid-b? [passport]
  (s/valid? :passport/passport passport))


(defn day4 [resource predicate]
  (->> (parse-input resource)
       (map predicate)
       (filter identity)
       (count)))

(deftest sample-a []
                  (is (= 2
                         (day4 "day4.sample.1.txt" valid-a?))))

(deftest sample-b []
                  (is (= 4
                         (day4 "day4.sample.2.txt" valid-b?))))

(run-tests 'day4.core)

