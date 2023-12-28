(ns util
  (:require [clojure.string :as str]))


(defn read-lines
  [file]
  (str/split-lines (slurp file)))


(defn parse-int
  [s]
  (Integer/parseInt s))
