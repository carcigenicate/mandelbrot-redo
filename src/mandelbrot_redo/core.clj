(ns mandelbrot-redo.core
  (:require [mandelbrot-redo.seesaw-main.second-main :as msm]

            [seesaw.core :as sc])

  (:gen-class))

(defn -main [& args]
  (-> (msm/new-frame)
      (sc/show!)))