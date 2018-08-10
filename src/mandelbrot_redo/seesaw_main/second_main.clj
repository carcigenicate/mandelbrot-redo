(ns mandelbrot-redo.seesaw-main.second-main
  (:require [seesaw.core :as sc]
            [seesaw.font :as sf]
            [seesaw.graphics :as sg]
            [seesaw.color :as s-col]

            [seesaw.dev :as sd]

            [mandelbrot-redo.logic.concurrent-finder :as mcf]
            [mandelbrot-redo.logic.async-result :as mar]
            [mandelbrot-redo.logic.bounds :as mb]
            [mandelbrot-redo.logic.coloring :as mc]
            [mandelbrot-redo.logic.helpers :as mh]

            [mandelbrot-redo.seesaw-main.helpers :as sh]
            [mandelbrot-redo.seesaw-main.key-handler :as mkh]
            [mandelbrot-redo.seesaw-main.recalculator :as mr]
            [mandelbrot-redo.seesaw-main.animation-helpers :as mah]

            [mandelbrot-redo.seesaw-main.sub-panels.color-scheme-panel :as mcsp]
            [mandelbrot-redo.seesaw-main.sub-panels.preset-panel :as mpp]
            [mandelbrot-redo.seesaw-main.sub-panels.canvas-panel :as mcp]
            [mandelbrot-redo.seesaw-main.saving.save-panel :as msp]
            [mandelbrot-redo.thread-pool :as pool])

  (:import [javax.swing Timer]
           [java.awt.event KeyEvent MouseEvent]
           (java.awt Component)))

; TODO: Make the color-multiples-atom hold a state that includes a history of previous states

(def loop-repaint-delay 500)

(def screen-ratio 0.5)
(def initial-screen-width 1200)
(def initial-screen-height (* initial-screen-width screen-ratio))

(def initial-mandel-bounds
  (mb/->Bounds -1 1, -1 1)
  #_
  (mb/->Bounds 0.3560738161423556 0.3560741611398823
               -0.6445325894528111 -0.6445322444552848))

(def initial-color-multiples
  [2.66 -3.29 2.67 3.34 4.47 -4.77 1.03 3.81 0.71])

(def multiple-steps (vec (take 9 (range 0 20 0.01))))

(defrecord UI-State [results-atom mandel-bounds-atom color-multiples-atom])

(defn new-ui-state [mandel-bounds color-multiples]
  (->UI-State (atom mar/new-async-pack) (atom mandel-bounds) (atom color-multiples)))

(defn start-repainting [repaint-delay canvas results-atom]
  (let [t (sc/timer
            (fn [_]
              (let [results @results-atom]
                (when (seq (:new-results results))
                  (sc/repaint! canvas))))
            :delay repaint-delay,
            :initial-delay repaint-delay)]

    (fn []
      (.stop ^Timer t))))
#_
(defn start-animating [change-delay canvas color-mutliples-atom]
  (let [t (sc/timer
            (fn [_]
              (swap! color-mutliples-atom
                ; TODO: Indicator that :current should be taken out and given to mah/advance?
                (fn [mults]
                  (let [anim-ranges (mah/from-steps 0 255 multiple-steps)]
                    (->> anim-ranges
                      (map #(assoc %2 :current %) mults)
                      (pmap mah/advance) ; FIXME: pmap?
                      (mapv :current)))))

              (sc/repaint! canvas))

            :initial-delay change-delay, :delay change-delay)]

    (fn []
      (.stop ^Timer t))))

(defn show-save-frame [ui-state]
  (let [save-frame (msp/new-save-frame ui-state)]
    (-> save-frame
        (sc/show!))))

(defn new-frame-panel [ui-state]
  (let [btn (sc/button :text "Open Save Window", :halign :center)
        panel (sc/horizontal-panel :items [btn])]

    (sc/listen btn
               :action (fn [_]
                         (show-save-frame ui-state)))

    panel))

(defn new-main-panel [ui-state]
  (let [main-panel (sc/border-panel)

        canvas (mcp/new-direction-wrapped-canvas ui-state)

        color-scheme-panel (mcsp/new-color-scheme-panel ui-state canvas)
        preset-panel (mpp/new-preset-panel ui-state main-panel)
        frame-panel (new-frame-panel ui-state)]

    (sc/config! main-panel :center canvas
                :east color-scheme-panel
                :west preset-panel
                :south frame-panel)

    main-panel))

#_
(defn key-handler [ui-state, canvas, ^KeyEvent e]
  (let [{:keys [results-atom mandel-bounds-atom]} ui-state
        changed? (atom false)] ; TODO: Eww
    (swap! mandel-bounds-atom
      #(if-let [new-state (mkh/alter-bounds-with-key? % (.getKeyCode e))]
         (do
           (reset! changed? true)
           new-state)

         %))

    (when @changed?
      (mr/start-calculating-points! results-atom @mandel-bounds-atom canvas))))

(defn new-frame []
  (let [{:keys [results-atom] :as ui-state}
        (new-ui-state initial-mandel-bounds
                       initial-color-multiples)

        main-panel (new-main-panel ui-state)

        ; TODO: :on-close? Default is :hide?
        frame (sc/frame :size [initial-screen-width :by initial-screen-height]
                        :content main-panel)

        canvas (sc/select frame [:#canvas])

        stop-repainting-f (start-repainting loop-repaint-delay canvas results-atom)]

    (sc/listen frame
       :window-closing (fn [_]
                         (stop-repainting-f)
                         (mar/stop-process @results-atom)
                         #_(pool/shutdown mcf/pool)
                         ;(stop-animating)

                         (println "Cleaned up...")))


    frame))
