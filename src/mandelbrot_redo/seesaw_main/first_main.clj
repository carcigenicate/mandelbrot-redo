(ns mandelbrot-redo.seesaw-main.first-main
  (:require [seesaw.core :as sc]
            [seesaw.font :as sf]
            [seesaw.graphics :as sg]
            [seesaw.color :as s-col]

            [seesaw.dev :as sd]

            [mandelbrot-redo.logic.concurrent-finder :as mcf]
            [mandelbrot-redo.logic.async-result :as mar]
            [mandelbrot-redo.logic.bounds :as mb]
            [mandelbrot-redo.seesaw-main.helpers :as sh]
            [mandelbrot-redo.logic.coloring :as mc])

  (:import [javax.swing Timer]))

(def point-division-perc 0.01)

(def loop-repaint-delay 1000)
(def resize-delay 750)

(def initial-mandel-bounds
  (mb/->Bounds 0.3553429385092607 0.3554179623077187 0.6426702348508788 0.6427452586493370))

(def color-f
  (mc/new-color-f (mc/->Multiples 21.45 -17.6 1.53
                                  16.76 3.2 20.71
                                  10.36 19.45 1.61)))

; TODO: Will need to make results-atom a more general state atom containing results?
; Have a record containg [results-atom mandel-bounds-atom]?

(defn start-calculating-points! [results-atom mandel-bounds canvas]
  (let [canvas-bounds (sh/bounds-from-component canvas)]
    (mcf/start-calculating-points! point-division-perc results-atom mandel-bounds canvas-bounds)))

(defn paint [results-atom cvs g]
  (let [chunks (mar/get-and-set-new-results! results-atom)]

    (when (seq chunks)
      (println "Painting" (count chunks) "chunks of" (count (first chunks))))

    ; TODO: Put in the when?
    (doseq [chunk chunks
            {[sx sy] :screen-coord, [mx my] :mandel-coord, i :iters} chunk]
      (sg/draw g
         (sg/rect sx sy 1)
         (sg/style :background (color-f mx my i))))))

(defn no-clear-paint [results-atom]
  {:before (partial paint results-atom),
   :super? false})

(defn new-canvas [results-atom]
  (let [canvas (sc/canvas :paint (no-clear-paint results-atom) #_(partial paint results-atom),
                          :id :canvas)

        t (sc/timer (fn [_]
                      (start-calculating-points! results-atom initial-mandel-bounds canvas)
                      (println "New bounds:" (into {} (sh/bounds-from-component canvas))))
                    :initial-delay resize-delay, :repeats? false, :start? false)]



    (sc/listen canvas
       :component-resized (fn [_]
                            (.restart ^Timer t)))

    canvas))

(defn start-repainting [repaint-delay canvas]
  (let [t (sc/timer
            (fn [_] (sc/repaint! canvas))
            :delay repaint-delay,
            :initial-delay repaint-delay)]

    (fn [] (.stop ^Timer t))))

(defn new-main-panel [results-atom]
  (let [canvas (new-canvas results-atom)

        main-panel (sc/border-panel :center canvas)]

    main-panel))


(defn new-frame []
  (let [results-atom (atom mar/new-async-pack)

        main-panel (new-main-panel results-atom)

        ; TODO: :on-close? Default is :hide?
        frame (sc/frame :size [500 :by 500]
                        :content main-panel)

        canvas (sc/select frame [:#canvas])

        stop-repainting-f (start-repainting loop-repaint-delay canvas)]

    (sc/listen frame
       :window-closing (fn [_]
                         (stop-repainting-f)
                         (mar/stop-process @results-atom)

                         (println "Cleaned up...")))

    frame))