(ns mandelbrot-redo.logic.coloring
  (:require [helpers.general-helpers :as g]))

(defrecord Multiples [x y i])
(defrecord Color-Multiples [red green blue])

(defn create-color-f [x-red-mult y-red-mult i-red-mult
                      x-green-mult y-green-mult i-green-mult
                      x-blue-mult y-mult-blue i-blue-mult]

  (letfn [(wrap [c] (g/wrap c 0 255))
          (calc-channel [c r g b] (wrap (+ (* c r) (* c g) (* c b))))]

    (fn [x y i]
      [(calc-channel x x-red-mult x-green-mult x-blue-mult)
       (calc-channel y y-red-mult y-green-mult y-mult-blue)
       (calc-channel i i-red-mult i-green-mult i-blue-mult)])))