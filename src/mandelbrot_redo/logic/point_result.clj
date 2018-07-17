(ns mandelbrot-redo.logic.point-result
  (:require [mandelbrot-redo.logic.helpers :as mh]))

; TODO: Give three fields: [mandel-coord screen-coord iters]
(defrecord Point-Result [coord iters])

(defrecord Point-Results [source-bounds results])

#_
(defn map-results [^Point-Results point-results, target-bounds]
  (let [{:keys [source-bounds results]} point-results]
    (->Point-Results
      target-bounds
      (mapv #(update % :coord mh/map-coord source-bounds target-bounds)
            results))))
