(ns mandelbrot-redo.logic.point-result
  (:require [mandelbrot-redo.logic.helpers :as mh]))

(defrecord Point-Result [mandel-coord screen-coord iters])

(defrecord Point-Results [source-bounds results])

(defn untested-coord-pair [mandel-coord screen-coord]
  (->Point-Result mandel-coord screen-coord nil))

(defn record-test [point-result test-result]
  (assoc point-result :iters test-result))

#_
(defn map-results [^Point-Results point-results, target-bounds]
  (let [{:keys [source-bounds results]} point-results]
    (->Point-Results
      target-bounds
      (mapv #(update % :coord mh/map-coord source-bounds target-bounds)
            results))))
