(ns mandelbrot-redo.seesaw-main.saving.save-panel
  (:require [seesaw.core :as sc]
            [mandelbrot-redo.logic.async-result :as mar]
            [mandelbrot-redo.logic.bounds :as mb]

            [mandelbrot-redo.seesaw-main.saving.save-state :as mss]
            [mandelbrot-redo.logic.concurrent-finder :as mcf]
            [mandelbrot-redo.seesaw-main.saving.result-drawer :as md]
            [mandelbrot-redo.logic.coloring :as mc]

            [mandelbrot-redo.seesaw-main.saving.time-estimator :as te]

            [mandelbrot-redo.thread-pool :as tp]

            [helpers.general-helpers :as g])

  (:import (javax.swing JButton)
           (java.awt.image BufferedImage)
           (java.awt Color)))

; TODO: Add time estimations.
; TODO: Add "Shut down after completing" checkboxes/one checkbox for when they all finish

(def finder-division-percentage 0.001)

(def save-width-ratio (double 2/3))
(def inital-save-width 300)

(def settings-font {:name "Arial", :size 20})

(def save-path "./mandel-saves/")

(def update-progress-bar-every-perc 0.001)

(def estimator-history-size 20)

(def test-ui-state {:results-atom         (atom mar/new-async-pack),
                    :mandel-bounds-atom   (atom (mb/->Bounds 0.3439489349071401 0.3439489363217023 -0.0560718303836418 -0.0560718289690796))
                    :color-multiples-atom (atom [1.12 -49.4 34.74 34.86 41.76 9.77 40.31 -12.66 -5.62])})

(defn ui-state->save-state
  "Returns a save state based on the current UI state.
  WARNING: Involved two unlocked dereferences of atoms.
    May have unpredicatable results if called while one of the atoms is being dereferenced."
  [ui-state]
  (let [{:keys [mandel-bounds-atom color-multiples-atom]} ui-state]
    (mss/->Save-State @mandel-bounds-atom @color-multiples-atom)))

(defn safe-save-name-now [raw-save-name]
  (str raw-save-name (int (g/current-ms-timestamp))))

(defn save-callback [progress-row update-every-perc result-n total-results]
  (let [update-every (int (* update-every-perc total-results))]
    (when (zero? (rem result-n update-every))
      (let [prog-bar (first (sc/select progress-row [:.draw-progress-bar]))
            t-remaining (first (sc/select progress-row [:.save-label]))
            est-atom (sc/user-data prog-bar)]

        (swap! est-atom te/add-snapshot result-n)

        (let [est @est-atom
              ms-remaining (te/estimated-ms-remaining est result-n total-results)]

          (sc/invoke-later
            (sc/text! t-remaining (te/format-ms ms-remaining 3))
            (sc/config! prog-bar :max total-results)
            (sc/value! prog-bar result-n)))))))

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

        result-chunks (mcf/interuptable-lazy-pmap-point-results
                        running?-atom
                        finder-division-percentage
                        mandel-bounds
                        display-bounds)

        color-f (mc/new-color-f color-multiples)

        raw-save-name (mss/serialize save-state)]

    ; TODO: Use own pool? Consequences of tying up one thread there?
    ;  Pool is used for canvas result production
    (tp/submit-task mcf/pool
       (let [img (md/produce-image-from-results
                   running?-atom
                   result-chunks
                   color-f
                   (mb/dimensions display-bounds)
                   (partial save-callback progress-row update-progress-bar-every-perc))]

         (when @running?-atom
           (md/save-image (safe-save-name-now raw-save-name) save-path img)
           (println "Saved!")

           (sc/invoke-later
             (let [container (sc/select save-root [:#progress-container])
                   btn (first (sc/select progress-row [:.cancel-button]))]
               (remove-progress-row! container btn))))))))

(defn new-progress-row []
  (let [prog-bar (sc/progress-bar :orientation :horizontal, :min 0,
                                  :class :draw-progress-bar)
        cancel-btn (sc/button :text "X", :background :red, :class :cancel-button,
                              :font settings-font, :valign :center)

        save-label (sc/label :font settings-font, :class :save-label)
        bar-panel (sc/grid-panel :columns 1, :items [save-label prog-bar])]

    (sc/config! prog-bar :user-data (atom (te/new-estimator estimator-history-size)))

    (sc/border-panel :center bar-panel, :east cancel-btn)))

(defn associate-prog-row-with-save! [progress-container progress-row running?-atom]
  (let [cancel-btn (first (sc/select progress-row [:.cancel-button]))]

    (sc/listen cancel-btn
              :action (fn [_] (reset! running?-atom false)
                              (remove-progress-row! progress-container cancel-btn)
                              (println "Cancelled!")))))

(defn save-handler [save-root save-state]
  (let [prog-row (new-progress-row)
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

    (sc/flow-panel :items [option-inputs save-btn], :id :settings-root)))

(defn new-progress-panel []
  (let [progress-bar-container (sc/vertical-panel :id :progress-container)]
    progress-bar-container))

(defn new-main-panel [ui-state]
  (let [main-panel (sc/vertical-panel :id :save-root)

        settings-panel (new-settings-panel main-panel ui-state)
        progress-panel (new-progress-panel)]

    (sc/add! main-panel settings-panel progress-panel)

    main-panel))

(defn stop-saving-all [root]
  (let [cancel-btns (sc/select root [:.cancel-button])]
    (doseq [^JButton btn, cancel-btns]
      (.doClick btn))))

(defn new-save-frame [ui-state]
  (let [main-panel (new-main-panel ui-state)
        frame (sc/frame :content main-panel, :size [500 :by 500])]

    (sc/listen frame
       :window-closing (fn [_]
                         (stop-saving-all main-panel)))

    frame))

(defn test-case []
  (let [w 6000
        h (int (* w 2/3))
        chunks (time (mcf/interuptable-lazy-pmap-point-results
                       (atom true)
                       0.001
                       (mb/->Bounds -1 1 -1 1)
                       (mb/from-dimensions w h)))

        img (BufferedImage. w h BufferedImage/TYPE_INT_RGB)

        color-f (mc/new-color-f [0.1 0.02 0.3, 0.04 0.5 0.06, 0.7 0.08 0.9])

        result-n-atom (atom 0)
        total-results (* w h)]

    (doto
      (Thread.
        ^Runnable
        (fn []
          (time
            (doseq [chunk chunks
                    {[mx my] :mandel-coord, [sx sy] :screen-coord, i :iters} chunk]
              (when (zero? (rem @result-n-atom 10000))
                (println (str (double (/ @result-n-atom total-results 0.01)) "%")))

              (.setRGB img sx sy (.getRGB ^Color (color-f mx my i)))

              (swap! result-n-atom inc)))

          (time
            (md/save-image "test5" "./" img))

          (println "Done")))

      (.start))))


