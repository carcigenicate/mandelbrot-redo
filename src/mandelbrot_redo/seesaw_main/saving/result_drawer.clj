(ns mandelbrot-redo.seesaw-main.saving.result-drawer
  (:require [mandelbrot-redo.logic.coloring :as mc]
            [mandelbrot-redo.logic.concurrent-finder :as mcf]
            [mandelbrot-redo.logic.bounds :as mb])

  (:import (java.awt.image BufferedImage)
           (java.awt Color)
           (javax.imageio ImageIO)
           (java.io File)))

(def image-type-ext "png")

; TODO: Completely seperate drawing and result production?
(defn produce-image-from-results [running?-atom result-chunks color-f image-dimensions callback-f]
  (let [[width height] image-dimensions
        total-results (apply * image-dimensions)
        ^BufferedImage img (BufferedImage. width height BufferedImage/TYPE_INT_RGB)

        result-n-atom (atom 0)]

    (doseq [chunk result-chunks
            {:keys [mandel-coord screen-coord iters]} chunk
            :let [[sx sy] screen-coord
                  [mx my] mandel-coord
                  color-int (.getRGB ^Color (color-f mx my iters))
                  result-n @result-n-atom]

            :while @running?-atom]

      (callback-f result-n total-results)

      (.setRGB img sx sy color-int)

      (swap! result-n-atom inc))

    img))

(defn save-image [^String save-name, ^String path, ^BufferedImage img]
  (let [path (str path "\\" save-name \. image-type-ext)]
    (clojure.java.io/make-parents path)
    (ImageIO/write img, ^String image-type-ext, (File. path))))