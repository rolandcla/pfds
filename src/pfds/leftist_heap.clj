(ns pfds.leftist-heap
  (:require [clojure.string :as str])
  (:require [pfds.protocols.heap :refer :all])
  )

(declare EMPTY make-leftist-heap)

(deftype LeftistHeap [rank x hl hr]
  HEAP
  (heap-empty? [heap] (identical? heap EMPTY))

  (heap-merge [h1 h2]
    (cond (or (nil? h1) (nil? (.x h1))) h2
          (or (nil? h2) (nil? (.x h2))) h1
          (<= (.x h1) (.x h2)) (make-leftist-heap (.x h1) (.hl h1) (heap-merge (.hr h1) h2))
          :else                (make-leftist-heap (.x h2) (.hl h2) (heap-merge h1 (.hr h2)))))

  (heap-insert [heap x]
    (heap-merge (LeftistHeap. 1 x EMPTY EMPTY) heap))

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

(def EMPTY (LeftistHeap. 0 nil nil nil))

(defmethod print-method LeftistHeap [h w]
  (if (heap-empty? h)
    (print-method 'EMPTY w)
    (print-method `(~'LeftistHeap. ~(.rank h) ~(.x h) ~(.hl h) ~(.hr h)) w)))

(defn leftist-heap-pp
  "LeftistHeap pretty print"
  ([heap] (println (leftist-heap-pp heap 1)))
  ([heap offset]
   (str 
        (if (heap-empty? heap) "-"
            (format "%-4d %s\n%s"
                    (.x heap)
                    (leftist-heap-pp (.hl heap) (inc offset))
                    (str (str/join (repeat (* 5 offset) \space))
                         (leftist-heap-pp (.hr heap) (inc offset))))))))

(extend-type nil
  HEAP
  (heap-insert [heap x] (LeftistHeap. 1 x EMPTY EMPTY)))

(defn- make-leftist-heap [x a b]
  (let [ra (.rank a)
        rb (.rank b)]
    (if (>= ra rb)
      (LeftistHeap. (inc rb) x a b)
      (LeftistHeap. (inc ra) x b a))))

