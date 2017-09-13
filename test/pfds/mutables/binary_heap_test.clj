(ns pfds.mutables.binary-heap-test
  (:require [pfds.mutables.binary-heap :refer :all]
            [clojure.test :refer :all]))

(deftest test-sorted
  (let [n 1000
        xs (repeatedly n #(rand-int 1000))
        heap (apply make-binary-heap Integer/TYPE n xs)
        ;;x (bh-del-min heap)
        ys (take-while identity (repeatedly #(bh-del-min heap)))
        ]
    (is (= ys (sort xs)))
    ;;(is (= (set ys) (set xs)))
    ;;(is (= x (first (sort xs))))
    ))

