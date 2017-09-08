(ns pfds.heap
  (:require  [clojure.test :as t]))

(declare EMPTY make-heap)

(defprotocol HEAP
  (heap-empty?     [heap])
  (heap-merge      [h1 h2])
  (heap-insert     [heap x])
  (heap-find-min   [heap])
  (heap-delete-min [heap]))

(deftype Heap [rank x hl hr]
  HEAP
  (heap-empty? [heap] (identical? heap EMPTY))
  (heap-merge [h1 h2]
    (cond (or (nil? h1) (nil? (.x h1))) h2
          (or (nil? h2) (nil? (.x h2))) h1
          (<= (.x h1) (.x h2)) (make-heap (.x h1) (.hl h1) (heap-merge (.hr h1) h2))
          :else                (make-heap (.x h2) (.hl h2) (heap-merge h1 (.hr h2)))))

  (heap-insert [heap x]
    (heap-merge (Heap. 1 x EMPTY EMPTY)
                heap))
  (heap-find-min [heap] (if (heap-empty? heap)
                          (throw (IllegalStateException. "Can't heap-find-min of an empty heap"))
                          (.x heap)))
  (heap-delete-min [heap] (if (heap-empty? heap)
                            (throw (IllegalStateException. "Can't heap-delete-min of an empty heap"))
                            (heap-merge (.hl heap) (.hr heap))))

  clojure.lang.IPersistentStack
  (cons  [heap x] (heap-insert heap x))
  (peek  [heap] (heap-find-min heap))
  (pop   [heap] (heap-delete-min heap))
  (count [heap] (+ 1 (count (.hl heap)) (count (.hr heap))))
  (seq   [heap] (lazy-seq (if (heap-empty? heap) nil
                              (cons (heap-find-min heap)
                                    (seq (heap-delete-min heap))))))
  (equiv [h1 h2] (= (seq h1) (seq h2)))
  )

(def EMPTY (Heap. 0 nil nil nil))

(defmethod print-method Heap [h w]
  (print-method 'Heap<- w)
    (print-method (seq h) w))


(extend-type nil
  HEAP
  (heap-insert [heap x] (Heap. 1 x EMPTY EMPTY)))

(defn- make-heap [x a b]
  (let [ra (.rank a)
        rb (.rank b)]
    (if (>= ra rb)
      (Heap. (inc rb) x a b)
      (Heap. (inc ra) x b a))))

