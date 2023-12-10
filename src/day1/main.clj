(ns day1.main
  (:require [clojure.string :as str]
            [util :as util]))


(defn get-first-and-last-number
  [line]
  (let [numbers (-> line
                    (str/replace #"\D" "")
                    (str/split #""))]
    (Integer/parseInt (str (first numbers) (last numbers)))))


(defn get-first-answer
  [file]
  (let [lines (util/read-lines file)]
    (->> lines
         (map get-first-and-last-number)
         (reduce + 0))))


(def str->number
  {"one"   1
   "1"     1
   "two"   2
   "2"     2
   "three" 3
   "3"     3
   "four"  4
   "4"     4
   "five"  5
   "5"     5
   "six"   6
   "6"     6
   "seven" 7
   "7"     7
   "eight" 8
   "8"     8
   "nine"  9
   "9"     9})


(defn words->numbers
  [line]
  (loop [line line
         numbers []]
    (if (empty? line)
      numbers
      (let [number (some (fn [[s n]] (when (str/starts-with? line s) n)) str->number)]
        (recur (apply str (rest line))
               (if number
                 (conj numbers number)
                 numbers))))))


(defn get-first-and-last-number-text
  [line]
  (let [numbers (words->numbers line)]
    (Integer/parseInt (str (first numbers) (last numbers)))))


(defn get-second-answer
  [file]
  (let [lines (util/read-lines file)]
    (->> lines
         (map get-first-and-last-number-text)
         (reduce + 0))))


(comment
  (get-first-answer "src/day1/example")
  (get-first-answer "src/day1/input")
  (get-second-answer "src/day1/input"))
