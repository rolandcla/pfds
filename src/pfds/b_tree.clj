(ns pfds.b-tree)

(declare a-insert a-split new-bnode)

(defprotocol B-TREE
  (bt-insert [this x]))

(deftype BTree [t o top]
  B-TREE
  (bt-insert [this x] (BTree. t o (bt-insert top x))))

(deftype BNode [n ks ns]
  B-TREE
  (bt-insert [this x] ))

(deftype BLeaf [n ks]
  B-TREE
  (bt-insert [this x]
    (if (< n (dec (alength ks)))
      (let [ks2 (a-insert ks n x)]
        (BLeaf. (inc n) ks2))
      (let [[a1 x a2] (a-split (a-insert ks n x))]
        (new-bnode a1 x a2)))))

(defn make-b-tree
  ([t o] {:pre [(odd? o)]} (BTree. t o (BLeaf. 0 (make-array t o)))))

(defmethod print-method BLeaf [this w]
  (print-method `(~'BLeaf. ~(.n this) ~(seq (.ks this))) w))

(defmethod print-method BTree [this w]
  (print-method `(~'BTree. ~(.t this) ~(.o this) ~(.top this)) w))

(defmethod print-method BNode [this w]
  (print-method `(~'BNode. ~(.n this) ~(seq (.ks this)) ~(seq (.ns this))) w))

(defn- a-insert [xs n x]
  (let [ys (aclone xs)]
    (loop [i n]
      (if (zero? i)
        (aset ys 0 x)
        (let [k (aget ys (dec i))]
          (if (< x k)
            (do (aset ys i k)
                (recur (dec i)))
            (aset ys i x)))))
    ys))

(defn- a-split [xs]
  (let [ys (aclone xs)
        n  (dec (alength ys))
        m  (dec (/ n 2))]
    (loop [n n m m]
      (when (>= m 0)
        (aset ys m (aget ys n))
        (recur (dec n) (dec m))))
    [xs (aget xs (inc m)) ys]))

(defn- new-bnode [a1 x a2]
  (let [ks (aclone a1)
        ns (object-array (alength a1))]
    (aset ks 0 x)
    (aset ns 0 (BLeaf. (quot (alength a1) 2) a1))
    (aset ns 1 (BLeaf. (quot (alength a2) 2) a2))
    (BNode. 1 ks ns)))
