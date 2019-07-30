(ns miner.taran
  (:require [miner.queens :as q]
            [tarantella.core :as t]))

;; For more information on Tarantella, see: https://github.com/Engelberg/tarantella
;;
;; In Tarantella terminology, each "row" is a candidate with certain columns set (marked 1).
;; Tarantella searches for a covering set of rows such that each column is set once in the
;; solution set.  For convenience, the constraint matrix can be specified as a set of marked
;; columns.
;;
;; For the N-Queens problem, we can encode our constraints for the positions of the queens.
;; In my previous solutions, I used the terms "row" and "column" to label the positions on
;; the board.  Chess players would probably prefer the terms "rank" and "file".  To avoid
;; confusion with the Tarantella terms, I will refer to the positions as X-Y coordinates on
;; an N-by-N board.  See my previous discussion for an explanation of how the N-Queens
;; problem is essentially concerned with finding unique positions in terms of X, Y, and the
;; two diagonals.  X and Y each have N possible values.  The diagonals constraints
;; correspond to the sums and differences of X and Y at each square.  Thus, there are 2N-1
;; possible values for each diagonal.
;;
;; For Tarantella, we want to designate each constraint as a separate column.  It's
;; convenient to encode X as the first N columns (0 to N-1), and Y as the next N columns (N to
;; 2N-1).  These constraint columns must be set exactly once to get unique placements in the
;; X and Y coordinates.  The diagonals are each assigned sequentially 2N-1 columns, but they
;; are optionals for Tarantella so they can be set once or not at all for a solution.
;;
;; The solutions returned by `dancing-links` are row numbers.  The constraints were
;; constructed so that the row numbers can be converted back into Y coordinates using
;; (rem ROW N).  That result follows the convention used in my other N-Queens solutions.
;;
;; See also:
;; https://en.wikipedia.org/wiki/Exact_cover
;;
;; The Wikipedia article points out that the diagonal constraints can be reduced slightly if
;; you exclude the four diagnols from the corner squares which never conflict.  I decided
;; the extra code to reduce the diagnonal constraints from 4N-2 to 4N-6 wasn't worth it.

(defn queens-constraints [n]
  (for [x (range n)
        y (range n)]
    [x (+ n y) (+ n n x y) (+ (* 5 n) (- x y 2))]))

;; The rows in a solution aren't guaranteed in any particular order so we need to sort first,
;; then decode queen placements from solution row numbers.
(defn solve-queens [n]
  (map (fn [sol] (map #(rem % n) (sort sol)))
       (t/dancing-links (queens-constraints n)
                        :optional-columns (set (range (* 2 n) (- (* 6 n) 2))))))

(defn sol= [a b]
  (and (= (count a) (count b))
       (= (set a) (set b))))

(defn smoke-test-taran []
  (assert (sol= (q/queens 8) (solve-queens 8)))
  true)
