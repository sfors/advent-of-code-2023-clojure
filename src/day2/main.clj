(ns day2.main
  (:require [clojure.string :as str]
            [util :as util]))


(defn parse-round-part
  [part]
  (let [[_ number color] (re-matches #"(\d+) (\w+)" part)]
    [(keyword color) (util/parse-int number)]))


(defn parse-round
  [round-str]
  (->> (str/split round-str #", ")
       (map parse-round-part)
       (reduce merge {})))


(defn parse-rounds
  [rounds-str]
  (->> (str/split rounds-str #"; ")
       (map parse-round)))


(defn parse-game
  [line]
  (let [[_ game-id rounds-str] (re-matches #"Game (\d+): (.*)" line)]
    {:game-id (util/parse-int game-id)
     :rounds  (parse-rounds rounds-str)}))


(defn valid-round?
  [{:keys [green blue red] :or {green 0 blue 0 red 0}}]
  (and (<= red 12)
       (<= green 13)
       (<= blue 14)))


(defn valid-game?
  [game]
  (every? valid-round? (:rounds game)))


(defn get-games
  [file]
  (->>
    (util/read-lines file)
    (map parse-game)))


(defn get-first-answer
  [file]
  (let [games (get-games file)]
    (reduce (fn [result game]
              (if (valid-game? game)
                (+ result (:game-id game))
                result))
            0
            games)))

(defn get-max-of-color
  [color rounds]
  (->> rounds
       (map color)
       (map (fn [x] (or x 0)))
       (apply max)))


(defn rounds->power
  [rounds]
  (let [green (get-max-of-color :green rounds)
        red (get-max-of-color :red rounds)
        blue (get-max-of-color :blue rounds)]
    (* green red blue)))


(defn get-second-answer
  [file]
  (let [games (get-games file)]
    (->> games
         (map (fn [game]
                (rounds->power (:rounds game))))
         (reduce +))))


(comment

  (get-first-answer "src/day2/example")
  (get-first-answer "src/day2/input")
  (get-second-answer "src/day2/example")
  (get-second-answer "src/day2/input")
  )
