(ns pfds.performance.check-heaps
  (:require [pfds.pairing-heap :as p-heap])
  (:require [pfds.binomial-heap :as b-heap])
  (:require [pfds.leftist-heap :as l-heap])
  (:require [pfds.mutables.binary-heap :as bin-heap]))

(defn check-pairing-heap [n]
  (println "check-pairing-heap" n)
  (let [seq1 (repeatedly n #(rand-int n))
        seq2 (repeatedly n #(rand-int n))
        _ (println "Populating heap with random ints")
        heap (time (into p-heap/EMPTY seq1))
        _ (println "Add and remove elements")
        heap (time (reduce #(-> %1 (conj %2) pop)
                           heap
                           seq2))

        seq1 (range n)
        seq2 (range n 0 -1)
        _ (println "\nPopulating heap with sorted ints")
        heap (time (into p-heap/EMPTY seq1))
        _ (println "Add and remove elements")
        heap (time (reduce #(-> %1 (conj %2) pop)
                           heap
                           seq1))
        ]))

(defn check-binomial-heap [n]
  (println "check-binomial-heap" n)
  (let [seq1 (repeatedly n #(rand-int n))
        seq2 (repeatedly n #(rand-int n))
        _ (println "Populating heap")
        heap (time (into b-heap/EMPTY seq1))
        _ (println "Add and remove elements")
        heap (time (reduce #(-> %1 (conj %2) pop)
                           heap
                           seq2))
        ]))

(defn check-leftist-heap [n]
  (println "check-leftist-heap" n)
  (let [seq1 (repeatedly n #(rand-int n))
        seq2 (repeatedly n #(rand-int n))
        _ (println "Populating heap")
        heap (time (into l-heap/EMPTY seq1))
        _ (println "Add and remove elements")
        heap (time (reduce #(-> %1 (conj %2) pop)
                           heap
                           seq2))
        ]))

(defn check-mut-binary-heap [n]
  (println "check-mut-binary-heap" n)
  (let [seq1 (repeatedly n #(rand-int n))
        seq2 (repeatedly n #(rand-int n))
        _ (println "Populating heap")
        heap (time (apply bin-heap/make-binary-heap Integer/TYPE (inc n) seq1))
        _ (println "Add and remove elements")
        heap (time (doseq [x seq2]
                     (bin-heap/bh-insert heap x)
                     (bin-heap/bh-del-min heap)))
        ]))

