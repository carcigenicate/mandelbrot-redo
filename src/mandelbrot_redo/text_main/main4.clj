(ns mandelbrot-redo.text-main.main4
  (:require [mandelbrot-redo.logic.bounds :as mb]
            [mandelbrot-redo.logic.concurrent-finder :as mcf]
            [mandelbrot-redo.logic.helpers :as mh]

            [helpers.general-helpers :as g]
            [lanterna.screen :as ls]

            [criterium.core :as cc]))

(def pixel-char \█)

(def term-width 300)
(def term-height 150)
(def term-font-size 3)
(def term-type :swing)

(def initial-mandelbrot-bounds
  (mb/map->Bounds {:min-x 0.34951178305690184, :max-x 0.36261898305727874,
                   :min-y -0.6510800875446726, :max-y -0.6379728875442955}))

(def initial-display-bounds
  (mb/->Bounds 0 term-width 0 term-height))

(def point-division-perc 0.001)

(defn color-for [iters]
  (cond
    (>= iters 200) :magenta
    (>= iters 150) :blue
    (>= iters 100) :cyan
    (>= iters 50) :green
    (>= iters 5) :yellow
    :else :red))

(defn draw-points [screen result-points color-f]
  (doseq [[{i :iters} [x y]] result-points]
    (ls/put-string screen x y (str pixel-char)
                   {:fg (color-f i)}))

  (ls/redraw screen))

(defn calculate-points [mandel-bounds display-bounds]
  (mcf/naive-point-results-par
    point-division-perc
    mandel-bounds
    display-bounds))

(defn draw-points-in [screen mandel-bounds display-bounds]
  (ls/clear screen)
  (ls/redraw screen)

  (let [points (time (calculate-points
                       mandel-bounds
                       display-bounds))]

    (draw-points screen points color-for)))

(defn new-screen [mandel-bounds-atom]
  (let [term-promise (promise)

        handler (fn [w h]
                  (draw-points-in @term-promise
                                  @mandel-bounds-atom
                                  (mb/from-dimensions w h)))

        term (ls/get-screen
               term-type
               {:cols term-width, :rows term-height,
                :font-size term-font-size,
                :resize-listener handler})]

    (deliver term-promise term)

    term))

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
      \h initial-mandelbrot-bounds

      nil)))


(defn get-last-key [screen]
  (loop [last-key nil]
    (if-let [new-key (ls/get-key screen)]
      (recur new-key)
      last-key)))

(defn display-bounds-from-screen [screen]
  (let [[w h] (ls/get-size screen)]
    (mb/from-dimensions w h)))

(defn start-key-listener [screen mandel-bounds-atom]
  (let [running-atom (atom true)

        routine (fn []
                  (while @running-atom
                    (Thread/sleep 500)
                    (swap! mandel-bounds-atom
                           (fn [b]
                             (let [k (get-last-key screen)]

                               ; Will it cause problems to stop in the middle of the routine?
                               (when (= k \q)
                                 (reset! running-atom false)
                                 (ls/stop screen))

                               (if-let [new-mandel (alter-with-key? b k)]
                                 (do
                                   (println "New:" (into {} new-mandel))
                                   (draw-points-in screen new-mandel
                                                   (display-bounds-from-screen screen))
                                   new-mandel)

                                 b))))))

        ^Thread t (Thread. ^Runnable routine)]

    (.start t)

    (fn [] (reset! running-atom false))))

;
(defn -main []
  (let [display-bounds initial-display-bounds
        mandel-bounds-atom (atom initial-mandelbrot-bounds)

        screen (new-screen mandel-bounds-atom)

        stop-key-listener-f (start-key-listener screen mandel-bounds-atom)]

    (ls/start screen)

    (draw-points-in screen @mandel-bounds-atom display-bounds)

    (fn [] (ls/stop screen)
           (stop-key-listener-f))))
