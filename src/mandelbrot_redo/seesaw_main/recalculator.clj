(ns mandelbrot-redo.seesaw-main.recalculator
  (:require [mandelbrot-redo.seesaw-main.helpers :as sh]
            [mandelbrot-redo.logic.concurrent-finder :as mcf]))

(def recalc-delay 750)
(def point-division-perc 0.05)

; A complicated mess because I want to limit how often it can be fired consecutively.
; It wouldn't be feasible to limit it at each call site, so the function delegates to a
;   callback given to new-non-repeatable-runner to ensure consecutive calls don't cause
;   multiple fires.

(let [recalc-f (sh/new-non-repeatable-runner recalc-delay
                  (fn [results-atom mandel-bounds canvas]
                    (let [canvas-bounds (sh/bounds-from-component canvas)]
                      (mcf/start-calculating-points! point-division-perc results-atom mandel-bounds canvas-bounds))))]

  (defn start-calculating-points!
    "Cancels any currently running operations attached to the results-atom, resets the result state, and begins
    calculating the points contained within the indicated mandel-bounds.
    Populates results-atom with the results."
    [results-atom mandel-bounds canvas]
    (recalc-f results-atom mandel-bounds canvas)))