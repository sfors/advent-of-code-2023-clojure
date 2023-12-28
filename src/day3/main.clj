(ns day3.main
  (:require [clojure.string :as str]
            [util :as util]))


(defn parse-symbols
  [lines]
  (->> lines
       (map-indexed
         (fn [row line]
           (map-indexed
             (fn [col character]
               (let [character (str character)]
                 (when (re-matches #"[^\d\.]" character)
                   [row col character])))
             line)))
       (apply concat)
       (filter identity)))


(defn parse-numbers-line
  [row line]
  (let [numbers (remove empty? (str/split line #"\D"))]
    (:result (reduce (fn [{:keys [from-index result]} number]
                       (let [col (str/index-of line number from-index)]
                         {:from-index (+ col (count number))
                          :result     (cons [row col number] result)}))
                     {:from-index 0
                      :result     []}
                     numbers))))


(defn parse-numbers
  [lines]
  (->> lines
       (map-indexed parse-numbers-line)
       (apply concat)
       ))


(defn is-next-to?
  [[number-row number-col number] [symbol-row symbol-col]]
  (and
    (<= (abs (- number-row symbol-row)) 1)
    (<= (dec number-col) symbol-col (+ number-col (count number)))))


(defn is-part-number?
  [number symbols]
  (some (fn [symbol] (is-next-to? number symbol)) symbols))


(defn parse-schematic
  [file]
  (let [lines (util/read-lines file)
        symbols (parse-symbols lines)
        numbers (->> lines
                     (parse-numbers)
                     (filter (fn [number] (is-part-number? number symbols))))]
    {:symbols symbols
     :numbers numbers}))


(defn get-first-answer
  [file]
  (let [{:keys [numbers]} (parse-schematic file)]
    (->> numbers
         (map (fn [[_ _ number]] number))
         (map util/parse-int)
         (reduce +))))


(defn symbol->gear
  [symbol numbers]
  (let [adj-part-numbers (->> numbers
                              (filter (fn [number] (is-next-to? number symbol)))
                              (filter identity))]
    (when (and (= (get symbol 2) "*")
               (= (count adj-part-numbers) 2))
      {:symbol           symbol
       :adj-part-numbers adj-part-numbers
       :ratio            (->> adj-part-numbers
                              (map (fn [[_ _ x]] (util/parse-int x)))
                              (reduce *))})))


(defn add-gears
  [{:keys [symbols numbers]}]
  {:symbols symbols
   :numbers numbers
   :gears   (->> symbols
                 (map (fn [symbol] (symbol->gear symbol numbers)))
                 (filter identity))}
  )


(defn get-second-answer
  [file]
  (let [schematic (add-gears (parse-schematic file))]
   (->> (:gears schematic)
        (map :ratio)
        (reduce +))
    )
  )


(comment
  (util/read-lines "src/day3/example")
  (parse-schematic "src/day3/example")
  (parse-schematic "src/day3/input")
  (get-first-answer "src/day3/example")
  (get-first-answer "src/day3/input")
  (get-second-answer "src/day3/example")
  (get-second-answer "src/day3/input")

  )
