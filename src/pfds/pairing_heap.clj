(ns pfds.pairing-heap
  (:require [clojure.string :as str])
  (:require [pfds.protocols.heap :refer :all]))

(declare merge-pairs)

(deftype PairingHeap [elem subheaps]
  HEAP
  (heap-empty? [_] (nil? elem))
  (heap-find-min [_]
    (if (nil? elem) (throw (IllegalStateException. "Can't heap-find-min of an empty heap"))
        elem))
  (heap-merge [this heap2]
    (cond (nil? elem)            heap2
          (heap-empty? heap2)    this
          (< elem (.elem heap2)) (PairingHeap. elem (conj subheaps heap2))
          :else                  (PairingHeap. (.elem heap2) (conj (.subheaps heap2) this))
          ))
  (heap-insert [this x] (heap-merge (PairingHeap. x []) this))
  (heap-delete-min [_]
    (if (nil? elem)
      (throw (IllegalStateException. "Can't heap-delete-min of an empty heap"))
      (merge-pairs subheaps)
      ))

  clojure.lang.IPersistentStack
  (equiv [this other] (and (= elem (.elem other))
                           (= subheaps (.subheaps other))))
  (cons [xs x] (.heap-insert xs x))
  (peek [xs]   (.heap_find_min xs))
  (pop  [xs]   (.heap_delete_min xs))
  (seq  [xs]   (if (.heap_empty? xs) nil (lazy-seq (cons (peek xs) (seq (pop xs))))))
  )

(def EMPTY (PairingHeap. nil []))

(defn- merge-pairs [heaps]
  (if-let [[h & hs] (seq heaps)]
    (if (seq hs)
      (heap-merge (heap-merge h (first hs)) (merge-pairs (next hs)))
      h)
    EMPTY))


