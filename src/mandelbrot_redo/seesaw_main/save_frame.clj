(ns mandelbrot-redo.seesaw-main.save-frame
  (:require [seesaw.core :as sc]
            [mandelbrot-redo.logic.async-result :as mar]
            [mandelbrot-redo.logic.bounds :as mb]))

(def settings-font {:name "Arial", :size 20})

(def test-ui-state {:results-atom         (atom mar/new-async-pack),
                    :mandel-bounds-atom   (atom (mb/from-dimensions 10 10))
                    :color-multiples-atom (atom [1 2 3 4 5 6 7 8 9])})

(defn save-handler [ui-state _])

(defn new-settings-panel [ui-state progress-atom]
  (let [label #(sc/label :text (str %), :font settings-font, :halign :center)
        width-input (sc/text :font settings-font, :id :width-input)
        ratio-input (sc/text :font settings-font, :id :ratio-input)

        option-grid (sc/vertical-panel :columns 2, :items [(label "Width") width-input
                                                           (label "Ratio") ratio-input])

        save-btn (sc/button :text "Save")]

    (sc/listen save-btn
       :action (partial save-handler))

    (sc/vertical-panel :items [option-grid save-btn])))

; TODO: How to track progresses to allow for cancellation/tracking multiple?
(defn new-progress-panel [])

(defn new-main-panel [ui-state]
  (let [progress-atom (atom [])]))

(defn new-save-frame [ui-state]
  (let [main-panel (new-settings-panel ui-state)
        frame (sc/frame :content main-panel, :size [1000 :by 1000])]
    frame))