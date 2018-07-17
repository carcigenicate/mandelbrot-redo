(ns mandelbrot-redo.logic.mandelbrot-iteration
  (:require [mandelbrot-redo.logic.complex-number :as cn]))

(def ^:dynamic *standard-test-limit* 200) ; FIXME: Too high?
(def ^:dynamic *standard-infinity-limit* 2)

(defn mandelbrot-iteration-f [seed-real seed-imag last-real last-imag]
  (let [[new-real new-imag] (cn/square-complex last-real last-imag)]
    [(+ new-real seed-real)
     (+ new-imag seed-imag)]))

(defn test-point-convergence [real imag max-iterations infinity-limit]
  (let [limit-sqrd (* infinity-limit infinity-limit)
        iter-f (partial mandelbrot-iteration-f real imag)

        under-limit? #(<= (+ (* % %)
                             (* %2 %2))
                          limit-sqrd)]
    (loop [real-acc real
           imag-acc imag
           iter 0]

      (if (and (< iter max-iterations)
               (under-limit? real-acc imag-acc))

        (let [[new-real new-imag] (iter-f real-acc imag-acc)]
          (recur new-real new-imag (inc iter)))

        iter))))

; TODO: Investigate why (mi/standard-mandelbrot-test-convergence 0 -4/5)
; TODO:  causes an accuracy explosion (ratio).

(defn standard-mandelbrot-test-convergence [real imag]
  (test-point-convergence real imag *standard-test-limit* *standard-infinity-limit*))