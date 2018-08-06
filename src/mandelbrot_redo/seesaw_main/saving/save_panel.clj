(ns mandelbrot-redo.seesaw-main.saving.save-panel
  (:require [seesaw.core :as sc]
            [mandelbrot-redo.logic.async-result :as mar]
            [mandelbrot-redo.logic.bounds :as mb]

            [mandelbrot-redo.seesaw-main.saving.save-state :as mss]
            [mandelbrot-redo.logic.concurrent-finder :as mcf]
            [mandelbrot-redo.seesaw-main.saving.result-drawer :as md]
            [mandelbrot-redo.logic.coloring :as mc]
            [helpers.general-helpers :as g]))

; FIXME How to cancel running jobs when the frame is closed?
; TODO: Add time estimations.
; TODO: Add "Shut down after completing" checkboxes/one checkbox for when they all finish

(def finder-division-percentage 0.01)

(def save-width-ratio (double 2/3))
(def inital-save-width 300)

(def settings-font {:name "Arial", :size 20})

(def save-path "./mandel-saves/")

; TODO: Change to a percentage of the total
(def update-progress-bar-every-perc 0.001)

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

(defn safe-save-name-now [raw-save-name]
  (str raw-save-name (int (g/current-ms-timestamp))))

(defn save-callback [progress-bar update-every-perc result-n total-results]
  (let [update-every (int (* update-every-perc total-results))]
    (when (zero? (rem result-n update-every))
      (sc/config! progress-bar :max total-results)
      (sc/value! progress-bar result-n)
      #_(println result-n (format "%.3f%%" (double (/ result-n total-results 0.01)))))))

(defn find-cancel-button-row? [progress-container target-cancel-button]
  (let [rows (sc/config progress-container :items)]
    (reduce
      (fn [default row]
        (let [cancel-button (first (sc/select row [:.cancel-button]))]
          (if (= cancel-button target-cancel-button)
            (reduced row)
            default)))
      nil
      rows)))

(defn remove-progress-row! [progress-container cancel-button]
  (if-let [row-to-remove (find-cancel-button-row? progress-container cancel-button)]
    (sc/remove! progress-container row-to-remove)

    (println "Warning! Row for" cancel-button "not found. THIS SHOULD NEVER HAPPEN!")))

(defn start-finding-process! [progress-row save-state running?-atom save-root]
  (let [{:keys [mandel-bounds color-multiples]} save-state
        width-input (sc/select save-root [:#width-input])

        width (sc/selection width-input)
        height (int (* width save-width-ratio))
        display-bounds (mb/from-dimensions width height)

        lazy-results (mcf/interuptable-lazy-naive-point-results-par
                       running?-atom
                       finder-division-percentage
                       mandel-bounds
                       display-bounds)

        color-f (mc/new-color-f color-multiples)

        raw-save-name (mss/serialize save-state)

        progress-bar (first (sc/select progress-row [:.draw-progress-bar]))]

    ; TODO: Submit to a pool
    (doto
      (Thread. ^Runnable
               (fn []
                 (let [img (md/produce-image-from-results
                             running?-atom
                             lazy-results
                             color-f
                             (mb/dimensions display-bounds)
                             (partial save-callback progress-bar update-progress-bar-every-perc))]

                   (when @running?-atom
                     (md/save-image (safe-save-name-now raw-save-name) save-path img)
                     (println "Saved!")

                     (sc/invoke-later
                       (let [container (sc/select save-root [:#progress-container])
                             btn (first (sc/select progress-row [:.cancel-button]))]
                         (remove-progress-row! container btn)))))))
      (.start))))

(defn new-progress-row [save-state]
  (let [prog-bar (sc/progress-bar :orientation :horizontal, :min 0,
                                  :class :draw-progress-bar)
        cancel-btn (sc/button :text "X", :background :red, :class :cancel-button,
                              :font settings-font, :valign :center)

        save-label (sc/label :font settings-font, :text (mss/serialize save-state))
        bar-panel (sc/grid-panel :columns 1, :items [save-label prog-bar])]

    (sc/border-panel :center bar-panel, :east cancel-btn)))

(defn associate-prog-row-with-save! [progress-container progress-row running?-atom]
  (let [cancel-btn (first (sc/select progress-row [:.cancel-button]))]

    (sc/listen cancel-btn
              :action (fn [_] (reset! running?-atom false)
                              (remove-progress-row! progress-container cancel-btn)
                              (println "Cancelled!")))))

(defn save-handler [save-root save-state]
  (let [prog-row (new-progress-row save-state)
        prog-container (sc/select save-root [:#progress-container])

        running?-atom (atom true)]

    (associate-prog-row-with-save! prog-container prog-row running?-atom)
    (start-finding-process! prog-row save-state running?-atom save-root)

    (sc/add! prog-container prog-row)))

(defn new-settings-panel [save-root ui-state]
  (let [label #(sc/label :text (str %), :font settings-font, :halign :center)
        width-input (sc/spinner :font settings-font, :id :width-input,
                                :model (sc/spinner-model inital-save-width
                                                         :from 10, :to, 75000 :by 250))

        option-inputs (sc/horizontal-panel :items [(label "Width") width-input])

        save-btn (sc/button :text "Save")]

    (sc/listen save-btn
       :action (fn [_]
                 (->> ui-state
                     (ui-state->save-state)
                     (save-handler save-root))))

    (sc/vertical-panel :items [option-inputs save-btn], :id :settings-root)))

(defn new-progress-panel []
  (let [progress-bar-container (sc/vertical-panel, :id :progress-container)]
    progress-bar-container))

(defn new-main-panel [ui-state]
  (let [main-panel (sc/grid-panel :columns 1)

        settings-panel (new-settings-panel main-panel ui-state)
        progress-panel  (new-progress-panel)]

    (sc/add! main-panel settings-panel progress-panel)

    main-panel))

(defn new-save-frame [ui-state]
  (let [main-panel (new-main-panel ui-state)
        frame (sc/frame :content main-panel, :size [1000 :by 1000])]
    frame))