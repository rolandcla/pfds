(ns pfds.pairing-heap-test
  (:require [pfds.pairing-heap :refer :all])
  (:require [pfds.protocols.heap :refer :all])
  (:import [pfds.pairing_heap PairingHeap])
  (:require [clojure.test :refer :all]))

(def heap1 (PairingHeap. 42 []))
(def heap2 (PairingHeap. 43 []))
(def heap1+2 (PairingHeap. 42 [heap2]))

(deftest test-heap-empty?
  (testing "With HEAP protocol"
    (is (heap-empty? EMPTY))
    (is (not (heap-empty? heap1))))
  (testing "With IPersistentStack protocol"
    (is (empty? EMPTY))
    (is (seq heap1))))

(deftest test-heap-find-min
  (testing "With HEAP protocol"
    (is (= 42 (heap-find-min heap1)))
    (is (thrown? IllegalStateException (heap-find-min EMPTY))))
  (testing "With IPersistentStack protocol"
    (is (= 42 (peek heap1)))
    (is (thrown? IllegalStateException (peek EMPTY)))))

(deftest test-heap-merge
  (testing "With HEAP protocol"
    (is (= heap1 (heap-merge EMPTY heap1)))
    (is (= heap1 (heap-merge heap1 EMPTY )))
    (is (= EMPTY (heap-merge EMPTY EMPTY)))
    (is (= heap1+2 (heap-merge heap1 heap2)))
    (is (= heap1+2 (heap-merge heap2 heap1)))
    ))

(deftest test-heap-insert
  (testing "With HEAP protocol"
    (is (= heap1 (heap-insert EMPTY 42)))
    (is (= heap1+2 (heap-insert heap1 43)))
    (is (= heap1+2 (heap-insert heap2 42)))
    )
  (testing "With IPersistentStack protocol"
    (is (= heap1 (conj EMPTY 42)))
    (is (= heap1+2 (conj heap1 43)))
    (is (= heap1+2 (conj heap2 42)))
    ))

(deftest test-heap-delete-min
  (testing "With HEAP protocol"
    (is (thrown? IllegalStateException (heap-delete-min EMPTY)))
    (is (= (heap-delete-min heap1) EMPTY))
    (is (= heap2 (heap-delete-min heap1+2)))
    )
  (testing "With IPersistentStack protocol"
    (is (thrown? IllegalStateException (pop EMPTY)))
    (is (= (pop heap1) EMPTY))
    (is (= heap2 (pop heap1+2)))
    ))

(deftest test-seq
  (let [xs (repeatedly 1000 #(rand-int 1000))
        heap (into EMPTY xs)]
    (is (= (seq heap) (sort xs)))))

