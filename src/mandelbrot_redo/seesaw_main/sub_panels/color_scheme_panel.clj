(ns mandelbrot-redo.seesaw-main.sub-panels.color-scheme-panel
  (:require [seesaw.core :as sc]
            [seesaw.dev :as sd]
            [helpers.general-helpers :as g]
            [mandelbrot-redo.seesaw-main.helpers :as sh]
            [mandelbrot-redo.logic.async-result :as mar])
  (:import (java.awt.event KeyEvent)))

(def input-font {:name "Arial", :size 30})

(def color-channel-types ["Red" "Green" "Blue"])
(def dimension-types ["x" "y" "i"])

(def initial-input-value "0")

(defn new-multiple-input [input-type-label]
  (let [label (sc/label :text input-type-label :font input-font, :halign :center)
        input (sc/text :text initial-input-value, :font input-font, :columns 4,
                       :class :multiple-input)]

    [label input]))

(defn new-color-channel-input []
  (let [multiple-inputs (mapcat new-multiple-input dimension-types)

        input-grid (sc/grid-panel :columns 2 :items multiple-inputs)]

    input-grid))

(defn multiples-from-inputs? [input-widgets]
  (let [parsed-multiples (mapv (comp sh/parse-double? sc/text) input-widgets)]
    (when (every? identity parsed-multiples)
      parsed-multiples)))

(defn update-colors-handler [ui-state canvas input-root]
  (let [{:keys [color-multiples-atom results-atom]} ui-state
        inputs (sc/select input-root [:.multiple-input])]

    (when-let [parsed-multiples (multiples-from-inputs? inputs)]
      (reset! color-multiples-atom parsed-multiples)

      (sh/force-result-repaint! canvas results-atom))))

(defn new-color-channels-input-panel [ui-state canvas]
  (let [channel-type-labels (map #(sc/label :text (str %), :font input-font,
                                            :valign :top, :halign :center)
                                 color-channel-types)
        channel-panels (interleave channel-type-labels (repeatedly #(new-color-channel-input)))
        color-mults-input-panel (sc/grid-panel :columns 2, :items channel-panels, :border 0)

        update-button (sc/button :text "Update", :font input-font)]

    (sc/listen update-button
               :action (fn [_]
                         (update-colors-handler ui-state canvas color-mults-input-panel)))

    (sc/vertical-panel :items [color-mults-input-panel update-button])))


(defn set-color-multiples [input-root new-color-mutliples]
  (let [inputs (sc/select input-root [:.multiple-input])
        pairs (mapv vector inputs new-color-mutliples)]

    (when-not (= (count inputs) (count new-color-mutliples))
      (throw (IllegalArgumentException.
               (str "Bad new-color-multiples. Recieved " new-color-mutliples
                    ". Expected " (count inputs) "."))))

    (doseq [[in new-n] pairs]
      (sc/text! in new-n))))

(defn new-color-scheme-panel [ui-state canvas]
  (let [{:keys [color-multiples-atom]} ui-state
        color-input-panel (new-color-channels-input-panel ui-state canvas)

        main-panel (sc/border-panel :east color-input-panel)]

    (set-color-multiples color-input-panel @color-multiples-atom)

    main-panel))

