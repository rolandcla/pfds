(ns pfds.protocols.heap)

(defprotocol HEAP
  (heap-empty?     [heap])
  (heap-merge      [h1 h2])
  (heap-insert     [heap x])
  (heap-find-min   [heap])
  (heap-delete-min [heap]))



