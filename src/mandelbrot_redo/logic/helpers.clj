(ns mandelbrot-redo.logic.helpers
  (:require [helpers.general-helpers :as g]
            [mandelbrot-redo.logic.bounds :as mb]

            [criterium.core :as cc]))

(defn map-coord [coord source-bounds target-bounds]
  (let [[x y] coord
        sb source-bounds
        tb target-bounds]
    [(double (g/map-range x, (:min-x sb) (:max-x sb), (:min-x tb) (:max-x tb)))
     (double (g/map-range y, (:min-y sb) (:max-y sb), (:min-y tb) (:max-y tb)))]))

(defn screen-coords-in [display-bounds]
  (let [{:keys [min-x max-x min-y max-y]} display-bounds]
    (for [y (range min-y max-y)
          x (range min-x max-x)]
      [x y])))

(defn mandel-display-coords-in
  "Returns a lazy list of [mandel-coord display-coord] pairs."
  [mandel-bounds display-bounds]
  (->> display-bounds
       (screen-coords-in)
       (map #(vector (map-coord % display-bounds mandel-bounds)
                     %))))

(defn interuptable-mandel-display-coords-in [running?-atom mandel-coords display-coords]
  (->> (mandel-display-coords-in mandel-coords display-coords)
       (take-while (fn [_] @running?-atom))))