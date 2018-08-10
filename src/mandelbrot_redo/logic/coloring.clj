(ns mandelbrot-redo.logic.coloring
  (:require [helpers.general-helpers :as g]
            [seesaw.color :as s-col]))

(defrecord Multiples [red-x red-y red-i, green-x green-y green-i, blue-x blue-y blue-i])

(defrecord Color-Scheme [color-f multiples])

(defn new-color-f
  "Returns a function that takes [x y i], and returns a color tuple of [r g b]."
  [multiples]
  (let [multiples' (if (sequential? multiples) (apply ->Multiples multiples) multiples)
        wrap #(g/wrap % 0 255)
        {:keys [red-x red-y red-i,
                green-x green-y green-i,
                blue-x blue-y blue-i]} multiples']
    (fn [x y i]
      (letfn [(calc-channel [xm ym im] (wrap (+ (* x xm) (* y ym) (* i im))))]
        (s-col/color (calc-channel red-x red-y red-i)
                     (calc-channel green-x green-y green-i)
                     (calc-channel blue-x blue-y blue-i))))))

; TODO: Remove? No point in storing color-f when it's relatively cheap to make.
(defn new-color-scheme [multiples]
  (->Color-Scheme (new-color-f multiples) multiples))

(defn apply-color-scheme [color-scheme x y i]
  ((:color-f color-scheme) x y i))