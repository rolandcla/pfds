(ns pfds.b-tree)

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
    (if (< n (alength ks))
      (let [ks2 (aclone ks)]
        (loop [i n]
          (if (zero? i)
            (aset ks2 0 x)
            (let [k (aget ks2 (dec i))]
              (if (< x k)
                (do (aset ks2 i k)
                    (recur (dec i)))
                (aset ks2 i x)))))
        (BLeaf. (inc n) ks2))
      (let [[s1 s2] (split-with #(< %1 x) ks)
            half    (inc (quot (alength ks) 2))
            [s1 s2] (split-at half (concat s1 [x] s2))
            a1 (into-array s1)
            a2 (into-array s2)
            ]
        [a1 a2]))))

(defn make-b-tree
  ([t o] {:pre [(even? o)]} (BTree. t o (BLeaf. 0 (make-array t (dec o))))))

(defmethod print-method BLeaf [this w]
  (print-method `(~'BLeaf. ~(.n this) ~(seq (.ks this))) w))

(defmethod print-method BTree [this w]
  (print-method `(~'BTree. ~(.t this) ~(.o this) ~(.top this)) w))

