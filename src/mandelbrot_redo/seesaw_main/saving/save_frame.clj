(ns mandelbrot-redo.seesaw-main.saving.save-frame
  (:require [seesaw.core :as sc]
            [mandelbrot-redo.logic.async-result :as mar]
            [mandelbrot-redo.logic.bounds :as mb]

            [mandelbrot-redo.seesaw-main.saving.save-state :as mss]
            [mandelbrot-redo.logic.concurrent-finder :as mcf]
            [mandelbrot-redo.seesaw-main.saving.result-drawer :as md]
            [mandelbrot-redo.logic.coloring :as mc]))

(def finder-division-percentage 0.01)

(def save-width-ratio (double 2/3))
(def inital-save-width 300)

(def settings-font {:name "Arial", :size 20})


(def test-ui-state {:results-atom         (atom mar/new-async-pack),
                    :mandel-bounds-atom   (atom (mb/->Bounds -1 1, -1 1))
                    :color-multiples-atom (atom [1.08 4.97 2.99 4.06 1.52 2.56 1.42 1.42 2.24])})

(defn ui-state->save-state
  "Returns a save state based on the current UI state.
  WARNING: Involved two unlocked dereferences of atoms.
    May have unpredicatable results if called while one of the atoms is being dereferenced."
  [ui-state]
  (let [{:keys [mandel-bounds-atom color-multiples-atom]} ui-state]
    (mss/->Save-State @mandel-bounds-atom @color-multiples-atom)))

(defn save-callback [print-every result-n total-results]
  (when (zero? (rem result-n print-every))
    (println result-n (format "%.3f%%" (double (/ result-n total-results 0.01))))))

(defn save-handler [ui-state settings-root _]
  (let [{:keys [mandel-bounds color-multiples] :as s-state} (ui-state->save-state ui-state)
        width-input (sc/select settings-root [:#width-input])

        width (sc/selection width-input)
        height (int (* width save-width-ratio))
        display-bounds (mb/from-dimensions width height)

        lazy-results (mcf/lazy-naive-point-results-par
                       finder-division-percentage
                       mandel-bounds
                       display-bounds)

        color-f (mc/new-color-f color-multiples)

        save-name (mss/serialize s-state)]

    ; TODO: Submit to a pool
    (doto
      (Thread. ^Runnable
               (fn []
                 (let [img (md/produce-image-from-results lazy-results
                                                          color-f
                                                          (mb/dimensions display-bounds)
                                                          (partial save-callback 100000))]
                   (md/save-image save-name "./" img)
                   (println "Saved!"))))
      (.start))))

(defn new-settings-panel [ui-state #_running-saves-atom]
  (let [label #(sc/label :text (str %), :font settings-font, :halign :center)
        width-input (sc/spinner :font settings-font, :id :width-input,
                                :model (sc/spinner-model inital-save-width
                                                         :from 10, :to, 75000 :by 250))

        option-inputs (sc/horizontal-panel :items [(label "Width") width-input])

        save-btn (sc/button :text "Save")]

    (sc/listen save-btn
       :action (partial save-handler ui-state option-inputs))

    (sc/vertical-panel :items [option-inputs save-btn])))

; TODO: Going to need to make the drawing and result production cancellable.
; Make a custom async-finder that uses pmap and is cancellable
; Have the draw routine accept a boolean atom that's checked once per chunk.
(defn new-progress-row [save-name total-results]
  (let [prog-bar (sc/progress-bar :orientation :horizontal, :min 0, :max total-results)]))

; TODO: How to track progresses to allow for cancellation/tracking multiple?
(defn new-progress-panel [running-saves-atom]
  (let []))

(defn new-main-panel [ui-state]
  (let [progress-atom (atom [])]))

(defn new-save-frame [ui-state]
  (let [main-panel (new-settings-panel ui-state)
        frame (sc/frame :content main-panel, :size [1000 :by 1000])]
    frame))