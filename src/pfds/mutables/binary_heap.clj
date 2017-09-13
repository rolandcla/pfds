(ns pfds.mutables.binary-heap)

(declare bh-insert)

(deftype BinaryHeap [ix xs])

(defn make-binary-heap
  ([t n] (BinaryHeap. (atom 0) (make-array t (inc n))))
  ([t n & xs] (let [heap (make-binary-heap t n)]
              (doseq [x xs] (bh-insert heap x))
              heap)))

(defn- swim [xs n]
  (when (> n 1)
    (let [m (quot n 2)
          xn (aget xs n)
          xm (aget xs m)]
      (when (< xn xm)
        (aset xs n xm)
        (aset xs m xn)
        (swim xs m)))))

(defn- sink [xs m ix]
  (let [n (* m 2)]
    (when (<= n ix)
      (let [xm (aget xs m)
            xn (aget xs n)
            [xn n] (if (< n ix)
                     (let [n1 (inc n)
                           xn1 (aget xs n1)]
                       (if (> xn xn1) [xn1 n1] [xn n]))
                     [xn n]
                     )]
        (when (< xn xm)
          (aset xs n xm)
          (aset xs m xn)
          (sink xs n ix))))))

(defn bh-insert [heap x]
  (let [ix (.ix heap)]
    (swap! ix inc)
    (aset (.xs heap) @ix x)
    (swim (.xs heap) @ix)
    nil))

(defn bh-del-min [heap]
  (let [ix @(.ix heap)]
    (when (> ix 0)
      (let [xs (.xs heap)
            x (aget xs 1)]
        (reset! (.ix heap) (dec ix))
        (aset xs 1 (aget xs ix))
        (sink xs 1 ix)
        x))))
