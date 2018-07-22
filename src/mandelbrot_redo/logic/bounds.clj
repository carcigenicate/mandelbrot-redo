(ns mandelbrot-redo.logic.bounds
  (:require [mandelbrot-redo.logic.helpers :as mh]))

(defrecord Bounds [min-x max-x min-y max-y])

; TODO: Safe to assume max > min? Use abs?
(defn width [bounds]
  (- (:max-x bounds) (:min-x bounds)))

(defn height [bounds]
  (- (:max-y bounds) (:min-y bounds)))

(defn dimensions [bounds]
  [(width bounds)
   (height bounds)])

(defn area [bounds]
  (->> bounds
       (dimensions)
       (apply *)))

(defn from-dimensions
  ([x-offset y-offset width height]
   (->Bounds x-offset (+ x-offset width)
             y-offset (+ y-offset height)))

  ([width height]
   (from-dimensions 0 0 width height))

  ([[w h :as dimensions]]
   (from-dimensions w h)))

(defn- to-each [bounds min-x-f max-x-f min-y-f max-y-f]
  (-> bounds
      (update :min-x min-x-f)
      (update :max-x max-x-f)
      (update :min-y min-y-f)
      (update :max-y max-y-f)))

(defn shift-by [bounds x-offset y-offset]
  (to-each bounds
           #(+ % x-offset) #(+ % x-offset)
           #(+ % y-offset) #(+ % y-offset)))

(defn move-to
  "[x y] is the top-right corner to move to."
  [bounds x y]
  (let [[w h] (dimensions bounds)]
    (from-dimensions x y w h)))

(defn adjust-size
  "Adds min-offset to the min bounds,
    and adds (- min-offset) to the max bounds."
  [bounds min-offset]
  (let [max-offset (- min-offset)]
    (to-each bounds
             #(+ % min-offset) #(+ % max-offset)
             #(+ % min-offset) #(+ % max-offset))))

(defn center-around [bounds x y]
  (let [[half-w half-h] (->> bounds
                             (dimensions)
                             (mapv #(double (/ % 2))))]

    (move-to bounds (- x half-w) (- y half-h))))
