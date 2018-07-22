(ns mandelbrot-redo.seesaw-main.second-main
  (:require [seesaw.core :as sc]
            [seesaw.font :as sf]
            [seesaw.graphics :as sg]
            [seesaw.color :as s-col]

            [seesaw.dev :as sd]

            [mandelbrot-redo.logic.concurrent-finder :as mcf]
            [mandelbrot-redo.logic.async-result :as mar]
            [mandelbrot-redo.logic.bounds :as mb]
            [mandelbrot-redo.seesaw-main.helpers :as sh]
            [mandelbrot-redo.seesaw-main.key-handler :as mkh]
            [mandelbrot-redo.logic.coloring :as mc]
            [mandelbrot-redo.logic.helpers :as mh]
            [mandelbrot-redo.seesaw-main.recalculator :as mr])

  (:import [javax.swing Timer]
           [java.awt.event KeyEvent MouseEvent]))

(def loop-repaint-delay 250)

(def resize-delay 500)

(def initial-mandel-bounds
  (mb/->Bounds 0.3560738161423556 0.3560741611398823
               -0.6445325894528111 -0.6445322444552848))

(def color-f
  (mc/new-color-f (mc/->Multiples 2.66 -3.29 2.67 3.34 4.47 -4.77 1.03 3.81 0.71)))

(defrecord UI-State [results-atom mandel-bounds-atom color-f-atom])

(defn new-ui-state [mandel-bounds color-f]
  (->UI-State (atom mar/new-async-pack) (atom mandel-bounds) (atom color-f)))

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

(defn mouse-press-handler [ui-state, canvas, ^MouseEvent e]
  (let [{:keys [results-atom mandel-bounds-atom]} ui-state
        canvas-bounds (sh/bounds-from-component canvas)]
    (swap! mandel-bounds-atom
           (fn [b]
             (let [zoom-by (->> b
                                (mb/dimensions)
                                (mapv #(double (/ % 2)))
                                (apply min))

                   zoom-mult (if (= (.getButton e) MouseEvent/BUTTON1) 0.6 -1)

                   [mx my] (mh/map-coord [(.getX e) (.getY e)] canvas-bounds b)]

               (-> b
                   (mb/center-around mx my)
                   (mb/adjust-size (* zoom-by zoom-mult))))))

    (mr/start-calculating-points! results-atom @mandel-bounds-atom canvas)))

(defn new-canvas [ui-state]
  (let [{:keys [results-atom mandel-bounds-atom]} ui-state
        canvas (sc/canvas :paint (no-clear-paint results-atom)
                          :id :canvas)

        resize-f (sh/new-non-repeatable-runner resize-delay
                    (fn []
                      (mr/start-calculating-points! results-atom @mandel-bounds-atom canvas)
                      (println "New bounds:" (into {} (sh/bounds-from-component canvas)))))]

    (sc/listen canvas
               :component-resized (fn [_] (resize-f))

               :mouse-pressed (partial mouse-press-handler ui-state canvas))

    canvas))

(defn start-repainting [repaint-delay canvas]
  (let [t (sc/timer
            (fn [_] (sc/repaint! canvas))
            :delay repaint-delay,
            :initial-delay repaint-delay)]

    (fn [] (.stop ^Timer t))))

(defn new-main-panel [ui-state]
  (let [canvas (new-canvas ui-state)

        main-panel (sc/border-panel :center canvas)]

    main-panel))

(defn key-handler [ui-state, canvas, ^KeyEvent e]
  (let [{:keys [results-atom mandel-bounds-atom]} ui-state
        changed? (atom false)]
    (swap! mandel-bounds-atom
           #(if-let [new-state (mkh/alter-bounds-with-key? % (.getKeyCode e))]
              (do
                (reset! changed? true)
                new-state)

              %))

    (when @changed?
      (mr/start-calculating-points! results-atom @mandel-bounds-atom canvas))))

(defn new-frame []
  (let [{:keys [results-atom] :as ui-state} (new-ui-state initial-mandel-bounds color-f)

        main-panel (new-main-panel ui-state)

        ; TODO: :on-close? Default is :hide?
        frame (sc/frame :size [500 :by 500]
                        :content main-panel)

        canvas (sc/select frame [:#canvas])

        stop-repainting-f (start-repainting loop-repaint-delay canvas)]

    (sc/listen frame
       :window-closing (fn [_]
                         (stop-repainting-f)
                         (mar/stop-process @results-atom)

                         (println "Cleaned up..."))

       :key-pressed (partial key-handler ui-state canvas))


    frame))