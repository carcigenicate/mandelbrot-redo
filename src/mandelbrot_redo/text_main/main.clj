(ns mandelbrot-redo.text-main.main
  (:require [mandelbrot-redo.logic.bounds :as mb]
            [mandelbrot-redo.logic.concurrent-finder :as mcf]
            [mandelbrot-redo.logic.helpers :as mh]
            [mandelbrot-redo.logic.async-result :as mar]
            [mandelbrot-redo.logic.coloring :as mc]

            [helpers.general-helpers :as g]
            [lanterna.screen :as ls]

            [criterium.core :as cc]
            [mandelbrot-redo.thread-pool :as pool]))

(def pixel-char \█)

(def term-width 300)
(def term-height 150)
(def term-font-size 3)
(def term-type :swing)

(def main-scheduled-pool (pool/new-scheduled-thread-pool (* 2 (pool/available-processors))))

(def draw-update-rate 250)
(def key-press-check-rate (* draw-update-rate 2))

(def initial-mandelbrot-bounds
  (mb/map->Bounds {:min-x 0.34951178305690184, :max-x 0.36261898305727874,
                   :min-y -0.6510800875446726, :max-y -0.6379728875442955}))

(def initial-display-bounds
  (mb/->Bounds 0 term-width 0 term-height))

(def point-division-perc 0.05)

(def colors [:magenta :blue :cyan :green :yellow :red])

(defn narrow-color-channels [color-triplet]
  (let [highest (* 255 3)
        sum (apply + color-triplet)
        perc (double (/ sum highest))
        i (int (* perc (count colors)))]

    (get colors i)))

(defn simple-color-f [_ _ iters]
  (cond
    (>= iters 200) :magenta
    (>= iters 150) :blue
    (>= iters 100) :cyan
    (>= iters 50) :green
    (>= iters 5) :yellow
    :else :red))

(def complex-color-f (fn [x y i] (let [xy-mult 50
                                       i-mult 5]
                                   (narrow-color-channels
                                     ((mc/create-color-f xy-mult xy-mult i-mult
                                                         xy-mult xy-mult i-mult
                                                         xy-mult xy-mult i-mult) x y i)))))

(defn draw-points [screen result-points color-f]
  (doseq [{[mx my] :mandel-coord,
           [sx sy] :screen-coord, i :iters} result-points]
    (ls/put-string screen sx sy (str pixel-char)
                   {:fg (color-f mx my i)}))

  (ls/redraw screen))

(defn start-calculating-points! [result-atom mandel-bounds display-bounds]
  (mar/stop-process @result-atom)
  (reset! result-atom mar/new-async-pack)

  (mcf/pool-async-point-results
    result-atom
    point-division-perc
    mandel-bounds
    display-bounds))

(defn draw-chunks [screen chunks]
  ; (ls/clear screen)
  ; (ls/redraw screen)

  (doseq [chunk chunks]
    (draw-points screen chunk simple-color-f)))

(defn display-bounds-from-screen [screen]
  (let [[w h] (ls/get-size screen)]
    (mb/from-dimensions w h)))

(defn draw-chunks-periodically
  ""
  [screen update-rate result-atom]
  (pool/submit-scheduled main-scheduled-pool update-rate
    (let [new-results (mar/get-and-set-new-results! result-atom)]
      (when (seq new-results)
        (println "Drawing" (count new-results))
        (draw-chunks screen new-results)))))

(defn new-screen [result-atom mandel-bounds-atom]
  (let [handler (fn [w h]
                  (start-calculating-points!
                                  result-atom
                                  @mandel-bounds-atom
                                  (mb/from-dimensions w h)))

        screen (ls/get-screen
                 term-type
                 {:cols      term-width, :rows term-height,
                  :font-size term-font-size})]

    (ls/add-resize-listener screen handler)

    screen))

(defn alter-with-key? [mandel-bounds key]
  (let [b mandel-bounds

        shift #(mb/shift-by b % %2)
        zoom #(mb/adjust-size b %)
        [hw hh] (mapv #(double (/ % 2)) (mb/dimensions b))
        zoom-by (min hw hh)]

    (case key
      (:left \a) (shift (- hw) 0)
      (:right \d) (shift hw 0)
      (:up \w) (shift 0 (- hh))
      (:down \s) (shift 0 hh)

      \z (zoom (* 0.5 zoom-by))
      \x (zoom (- zoom-by))

      \e mandel-bounds ; So we can refresh the color without closing
      \h initial-mandelbrot-bounds

      nil)))

(defn get-last-key
  "Clears the buffer, and returns the last key press that was present."
  [screen]
  (loop [last-key nil]
    (if-let [new-key (ls/get-key screen)]
      (recur new-key)
      last-key)))

(defn start-key-listener
  "Starts listening for key-presses.
  Returns a 0-arity function that stops the listening when called."
  [result-atom screen mandel-bounds-atom stop-f-promise]
  (let [routine (fn []
                  (swap! mandel-bounds-atom
                         (fn [b]
                           (let [k (get-last-key screen)]

                             (when (= k \q)
                               (@stop-f-promise))

                             (if-let [new-mandel (alter-with-key? b k)]
                               (do
                                 (println "New:" (into {} new-mandel))
                                 (start-calculating-points! result-atom new-mandel
                                                            (display-bounds-from-screen screen))
                                 new-mandel)

                               b)))))]

    (pool/submit-scheduled* main-scheduled-pool key-press-check-rate routine)))

;
(defn -main []
  (let [mandel-bounds-atom (atom initial-mandelbrot-bounds)
        starting-mandel-bounds @mandel-bounds-atom

        result-atom (atom mar/new-async-pack)

        stop-f-promise (promise)

        screen (new-screen result-atom mandel-bounds-atom)

        display-bounds (display-bounds-from-screen screen)

        stop-key-listener-f (start-key-listener result-atom screen mandel-bounds-atom stop-f-promise)
        stop-draw-f (draw-chunks-periodically screen draw-update-rate result-atom)
        stop-f (fn []
                 (ls/stop screen)
                 (stop-key-listener-f)
                 (stop-draw-f)
                 (mar/stop-process @result-atom)
                 (println "Stopped all..."))]

    (deliver stop-f-promise stop-f)

    (ls/start screen)

    (.addShutdownHook (Runtime/getRuntime) (Thread. ^Runnable stop-f))

    (start-calculating-points! result-atom starting-mandel-bounds display-bounds)

    stop-f))