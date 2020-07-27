(ns miner.taran
  (:require [miner.queens :as q]
            [tarantella.core :as t]))

;; For more information on Tarantella, see: https://github.com/Engelberg/tarantella

;; I wrote a blog post about this code:
;; http://conjobble.velisco.com/2019/07/30/tarantella-queens.html

(defn queens-constraints [n]
  (for [x (range n)
        y (range n)]
    #{x (+ n y) (+ n n x y) (+ (* 5 n) (- x y 2))}))

;; The rows in a solution aren't guaranteed in any particular order so we need to sort first,
;; then decode queen placements from solution row numbers.
(defn solve-queens [n]
  (map (fn [sol] (mapv #(rem % n) (sort sol)))
       (t/dancing-links (queens-constraints n)
                        :optional-columns (set (range (* 2 n) (- (* 6 n) 2))))))

(defn sol= [a b]
  (and (= (count a) (count b))
       (= (set a) (set b))))

(defn smoke-test-taran []
  (assert (sol= (q/queens 8) (solve-queens 8)))
  true)
