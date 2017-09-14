(ns pfds.binomial-heap
  (:require  [clojure.string :as str]))

(declare insert-tree ts-merge remove-min-tree)

(deftype Node [r x cs])

(deftype Heap [ts]
  clojure.lang.IPersistentStack
  (cons [_ x] (Heap. (insert-tree (Node. 0 x '()) ts)))
  (peek [_] (let [[t _] (remove-min-tree ts)] (.x t)))
  (pop  [_] (Heap. (let [[t ts2] (remove-min-tree ts)]
                     (ts-merge (reverse (.cs t)) ts2))))
  (seq  [h] (when (seq ts)
              (cons (peek h) (seq (pop h)))))
  )

(def EMPTY (Heap. '()))

(defn make-binomial-heap
  ([] EMPTY)
  ([& xs] (into EMPTY xs)))

(defn bh-merge [h1 h2]
  (Heap. (ts-merge (.ts h1) (.ts h2))))

(defmethod print-method Node [n w]
  (print-method `(~'Node. ~(.r n) ~(.x n) ~(if (seq (.cs n)) `(~'list ~@(.cs n)) ())) w))

(defmethod print-method Heap [h w]
  (print-method `(~'Heap. (~'list ~@(.ts h))) w))

(defn- node-pretty-print
  ([n] (node-pretty-print n nil 0))
  ([n hdr fill]
   (println (or hdr "")
            (str/join (when-not hdr (repeat fill \space)))
            "Node" (.r n) (.x n))
   (doseq [c (.cs n)] (node-pretty-print c nil (+ 3 fill)))))

(defn bh-pretty-print [h]
  (doseq [t (.ts h)] (node-pretty-print t "#" 0)))

;;----------------------------------------------------------------------

(defn- link [t1 t2]
  (if (<= (.x t1) (.x t2))
    (Node. (inc (.r t1)) (.x t1) (conj (.cs t1) t2))
    (Node. (inc (.r t1)) (.x t2) (conj (.cs t2) t1))))

(defn- insert-tree [t ts]
  ;;(println "insert-tree" t ts)
  (if (seq ts)
    (let [u (first ts)]
      (if (< (.r t) (.r u))
        (conj ts t)
        (insert-tree (link t u)
                     (rest ts))))
    (list t)))

(defn- ts-merge [tts1 tts2]
  ;;(println "ts-merge" tts1 tts2)
  (cond (empty? tts1) tts2
        (empty? tts2) tts1
        :else
        (let [[t1 & ts1] tts1
              [t2 & ts2] tts2]
          (cond (< (.r t1) (.r t2)) (conj (ts-merge ts1 tts2) t1)
                (> (.r t1) (.r t2)) (conj (ts-merge tts1 ts2) t2)
                :else (insert-tree (link t1 t2) (ts-merge ts1 ts2))))))

(defn- remove-min-tree [tts]
  ;;(println "remove-min-tree" tts)
  (if-let [[t & ts] (seq tts)]
    (if (empty? ts) [t '()]
        (let [[t2 ts2] (remove-min-tree ts)]
          (if (<= (.x t) (.x t2))
            [t ts]
            [t2 (conj ts2 t)])))
    (throw (IllegalStateException. "Can't remove-min-tree from empty heap"))))
