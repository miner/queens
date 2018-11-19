(ns miner.test-queens
  (:require [clojure.test :refer :all]
            [miner.queens :refer :all]))


(deftest eight-queens []
  (let [dim 8
        answer-set (set (queens dim))]
    (doseq [qf [rosetta-queens queens richards-queens]]
      (is (= answer-set (set (qf dim)))))))

