(ns pfds.red-black-tree
  (:require [clojure.core.match :refer [match]]))

(defn- balance-l [rbt]
  (match rbt
         [:black [:red [:red l2 v2 r2] v1 r1] v0 r0] [:red [:black l2 v2 r2] v1 [:black r1 v0 r0]]
         [:black [:red l1 v1 [:red l2 v2 r2]] v0 r0] [:red [:black l1 v1 l2] v2 [:black r2 v0 r0]]
         :else                                       rbt))

(defn- balance-r [rbt]
  (match rbt
         [:black l0 v0 [:red l1 v1 [:red l2 v2 r2]]] [:red [:black l0 v0 l1] v1 [:black l2 v2 r2]]
         [:black l0 v0 [:red [:red l2 v2 r2] v1 r1]] [:red [:black l0 v0 l2] v2 [:black r2 v1 r1]]
         :else                                       rbt))

(defn- black? [rbt]
  (or (nil? rbt)
      (= :black (first rbt))))

(defn children-of-red-are-black? [rbt]
  (match rbt
         nil true
         [:black l _ r] (every? children-of-red-are-black? [l r])
         [:red   l _ r] (and (every? black? [l r])
                             (every? children-of-red-are-black? [l r]))))

(defn black-height [rbt]
  (match rbt
         nil 0
         [c l _ r] (if-let [lh (black-height l)]
                     (if-let [rh (black-height r)]
                       (and (= lh rh)
                            (if (= :black c) (inc lh) lh))
                       false)
                     false)))

(defn rbt-insert [rbt x]
  (letfn
      [(ins [rbt x]
         (if-let [[c l v r] rbt]
           (cond (< x v) (balance-l [c (ins l x) v r])
                 (> x v) (balance-r [c l v (ins r x)])
                 :else   rbt)
           [:red nil x nil]
           ))]
    (let [[_ l v r] (ins rbt x)]
      [:black l v r])))

(defn rbt-find [rbt x]
  (if-let [[_ l v r] rbt]
    (cond (< x v) (recur l x)
          (> x v) (recur r x)
          :else   x)))

(defn rbt-seq [rbt]
  (if-let [[_ l v r] rbt]
    (concat (rbt-seq l) (cons v (rbt-seq r)))))

(defn rbt-subseq [rbt x]
  (if-let [[_ l v r] rbt]
    (cond (<= x v) (concat (rbt-subseq l x)
                          (cons v (rbt-seq r)))
          :else   (recur r x))))
