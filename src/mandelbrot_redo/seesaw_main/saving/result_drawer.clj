(ns mandelbrot-redo.seesaw-main.saving.result-drawer
  (:require [mandelbrot-redo.logic.coloring :as mc]
            [mandelbrot-redo.logic.concurrent-finder :as mcf]
            [mandelbrot-redo.logic.bounds :as mb])

  (:import (java.awt.image BufferedImage)
           (java.awt Color)
           (javax.imageio ImageIO)
           (java.io File)))

(def image-type-ext "png")

; (println result-n (format "%.3f%%" (double (/ result-n total-results 0.01)))))

; TODO: Add a callback that passes the total for progress updates?
; How am I going to find how many results there are?
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

      ; TODO: Remove try. Only to diagnose OOB errors.
      (try
        (.setRGB img sx sy color-int)

        (catch ArrayIndexOutOfBoundsException e
          (println "AIOOB: " image-dimensions [sx sy] [mx my])))

      (swap! result-n-atom inc))

    img))

(defn save-image [^String save-name, ^String path, ^BufferedImage img]
  (let [path (str path "\\" save-name \. image-type-ext)]
    (clojure.java.io/make-parents path)
    (ImageIO/write img, ^String image-type-ext, (File. path))))

#_(let [w 1500
        image-dims [w (int (* w 2/3))]
        results (mcf/lazy-naive-point-results-par
                  0.1
                  (mb/->Bounds -2 2, -2 2)
                  (mb/from-dimensions image-dims))

        color-f (mc/new-color-f [1 2 3 4 5 6 7 8 9])

        img (produce-image-from-results
              results
              color-f
              image-dims)]

    (println "Saving...")

    (save-image "testImg" ".\\" img))