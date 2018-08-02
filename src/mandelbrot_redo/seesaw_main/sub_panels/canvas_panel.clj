(ns mandelbrot-redo.seesaw-main.sub-panels.canvas-panel
  (:require [mandelbrot-redo.seesaw-main.helpers :as sh]
            [seesaw.graphics :as sg]
            [mandelbrot-redo.logic.bounds :as mb]
            [mandelbrot-redo.logic.helpers :as mh]
            [mandelbrot-redo.seesaw-main.recalculator :as mr]
            [seesaw.core :as sc]
            [mandelbrot-redo.logic.coloring :as mc]
            [mandelbrot-redo.logic.async-result :as mar])
  (:import (java.awt.event MouseEvent)))

(def resize-delay 500)
(def offset-dimension-perc 0.5)

(def direction-font {:name "Arial", :size 35})

(defn paint [ui-state cvs g]
  (let [{:keys [results-atom color-multiples-atom]} ui-state

        chunks (mar/mark-and-get-all! results-atom)

        color-mults @color-multiples-atom
        color-f (mc/new-color-f color-mults)]

    (when (seq chunks)
      (println "Painting" (count chunks) "chunks of" (count (first chunks))))

    ; TODO: Put in the when?
    (doseq [chunk chunks
            {[sx sy] :screen-coord, [mx my] :mandel-coord, i :iters} chunk]
      (sg/draw g
               (sg/rect sx sy 1)
               (sg/style :background (color-f mx my i))))))

(defn no-clear-paint [results-atom]
  {:after (partial paint results-atom),
   :super? true})

(defn mouse-press-handler [ui-state, canvas, ^MouseEvent e]
  (let [{:keys [results-atom mandel-bounds-atom]} ui-state
        canvas-bounds (sh/bounds-from-component canvas)]
    (swap! mandel-bounds-atom
           (fn [b]
             (let [zoom-by (->> b
                                (mb/dimensions)
                                (mapv #(double (/ % 2)))
                                (apply min))

                   zoom-mult (if (= (.getButton e) MouseEvent/BUTTON1) 0.9 -1)

                   [mx my] (mh/map-coord [(.getX e) (.getY e)] canvas-bounds b)]

               (-> b
                   (mb/center-around mx my)
                   (mb/adjust-size (* zoom-by zoom-mult))))))

    (mr/start-calculating-points! results-atom @mandel-bounds-atom canvas)))

(defn new-canvas [ui-state]
  (let [{:keys [results-atom mandel-bounds-atom]} ui-state
        canvas (sc/canvas :paint (no-clear-paint ui-state)
                          :id :canvas)

        resize-f (sh/new-non-repeatable-runner resize-delay
                    (fn []
                      (mr/start-calculating-points! results-atom @mandel-bounds-atom canvas)
                      (println "New bounds:" (into {} (sh/bounds-from-component canvas)))
                      (println "Frame dims:" (sh/dimensions-of-component
                                               (sh/get-root-of canvas)))))]

    (sc/listen canvas
               :component-resized (fn [_] (resize-f))

               :mouse-pressed (partial mouse-press-handler ui-state canvas))

    canvas))

(defn new-direction-button [canvas ui-state label-text x-offset-mult y-offset-mult]
  (let [{:keys [mandel-bounds-atom results-atom]} ui-state
        btn (sc/button :text (str label-text),
                       :background :white, :border 10, :font direction-font)]

    (sc/listen btn
       :action (fn [_]
                 (let [new-bounds
                       (swap! mandel-bounds-atom
                              (fn [b]
                                (let [[width-off height-off]
                                      (->> b
                                           (mb/dimensions)
                                           (map #(* % offset-dimension-perc)))]

                                  (mb/shift-by b (* width-off x-offset-mult)
                                                 (* height-off y-offset-mult)))))]

                   (mr/start-calculating-points! results-atom new-bounds canvas))))

    btn))

(defn new-direction-wrapped-canvas [ui-state]
  (let [canvas (new-canvas ui-state)

        btn #(new-direction-button canvas ui-state % %2 %3)]

    (sc/border-panel :center canvas

                     :north (btn "↑" 0 -1)
                     :south (btn "↓" 0 1)
                     :west (btn "←" -1 0)
                     :east (btn "→" 1 0))))
