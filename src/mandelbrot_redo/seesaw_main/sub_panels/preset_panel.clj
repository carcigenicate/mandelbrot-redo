(ns mandelbrot-redo.seesaw-main.sub-panels.preset-panel
  (:require [mandelbrot-redo.seesaw-main.presets.colors :as pre-c]
            [mandelbrot-redo.seesaw-main.presets.helpers :as pre-h]
            [mandelbrot-redo.seesaw-main.helpers :as sh]

            [mandelbrot-redo.seesaw-main.sub-panels.color-scheme-panel :as mcsp]

            [seesaw.core :as sc]))

(def preset-font {:name "Arial", :size 20})

(defn new-preset-selector [preset-label preset-map selection-callback]
  (let [pretty->original (into {} (map (fn [[k _]] [(pre-h/pretty-preset-name k) k])
                                       preset-map))

        selector (sc/combobox :model (keys pretty->original), :font preset-font)

        label (sc/label :text (str preset-label), :font preset-font, :halign :center)]

    (sc/listen selector
               :selection
               (fn [_]
                 (let [pretty-k (sc/selection selector)]
                   (selection-callback pretty-k (preset-map (pretty->original pretty-k))))))

    (sc/flow-panel :items [label selector])))

(defn color-handler [ui-state root-panel _ color-multiples]
  (let [{:keys [color-multiples-atom results-atom]} ui-state
        canvas (sc/select root-panel [:#canvas])]

    (reset! color-multiples-atom color-multiples)
    (sh/force-result-repaint! canvas results-atom)
    (mcsp/set-color-multiples root-panel color-multiples)))

(defn new-preset-panel [ui-state root-panel]
  (let [color-selector (new-preset-selector "Colors"
                                            pre-c/standard-preset-colors
                                            (partial color-handler ui-state root-panel))]

    (sc/vertical-panel :items [color-selector])))
