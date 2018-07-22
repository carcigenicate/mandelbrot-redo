(ns mandelbrot-redo.irrelevant.boilerplate-question
  (:require [seesaw.core :as sc]
            [seesaw.font :as sf]
            [seesaw.graphics :as sg]

            [seesaw.dev :as sd]))

(defn paint [state-atom cvs g]
  (println cvs "\n" g))

(defn new-canvas [state-atom]
  (let [cvs (sc/canvas :paint (partial paint state-atom))]

    cvs))


(defn new-main-panel [state-atom]
  (let [cvs (new-canvas state-atom)

        main-panel (sc/border-panel :center cvs)]

    main-panel))


(defn new-frame []
  (let [results-atom (atom [])

        main-panel (new-main-panel results-atom)

        frame (sc/frame :size [500 :by 500]
                        :content main-panel)]

    frame))