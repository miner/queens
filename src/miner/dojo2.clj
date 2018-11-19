;; Original https://github.com/Azel4231/muc-codingdojo-queens
;; blog: https://feierabendprojekte.wordpress.com/2018/10/27/high-level-vs-low-level/
;; https://en.wikipedia.org/wiki/Eight_queens_puzzle


;; SEM severly hacked version

(ns miner.dojo2
  "Hacked by SEM, cleaner version."
  (:require [clojure.pprint :as p]
            [clojure.math.combinatorics :as c]))


;; higher order function that allows expressing the rules more concisely
(defn unique? [f]
  (fn [coll] (apply distinct? (map f coll))))

;; function overloading to make code below more readable
(def y-coord second)
(def diagonal1 (partial apply +))
(def diagonal2 (partial apply -))

;; SEM: I had to think about the diagonals for a minute.  Adding coordinates x+y gives you a
;; sort of "slope constant" going diagonally one way.  Subtracting x-y gives you the other
;; diagonal.


(def valid?
  (every-pred (unique? y-coord)
              (unique? diagonal1)
              (unique? diagonal2)))


;; Place the queens to the board. Use the x/y tuples as "path" into the two-dimensional array (assoc-in does this).
(defn queens [dimension]
  (let [all-positions (for [x (range dimension)]
                        (for [y (range dimension)]
                          [x y]))
        all-combinations  (apply c/cartesian-product all-positions)]
    (filter valid? all-combinations)))


(defn add-to-board [board positions]
  (reduce #(assoc-in %1 %2 "Q")
          board
          positions))

(defn print-board [board]
  (println)
  (doseq [row board]
    (println (apply str row))))

  
(defn print-solutions [dimension solutions]
  (let [empty-line (into [] (repeat dimension "+"))
        empty-board (into [] (repeat dimension empty-line))]
    (println (count solutions) " results (" dimension "x" dimension "):")
    (doseq [sol (map #(add-to-board empty-board %) solutions)]
      (print-board sol))
    (println)))

(defn run-queens [dimension]
  (print-solutions dimension (queens dimension)))

