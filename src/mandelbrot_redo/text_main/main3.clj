(ns mandelbrot-redo.text-main.main3
  (:require [mandelbrot-redo.logic.bounds :as mb]
            [mandelbrot-redo.logic.mandelbrot-iteration :as mi]
            [mandelbrot-redo.logic.point-result :as mpr]

            [helpers.general-helpers :as g]

            [lanterna.terminal :as lt]
            [mandelbrot-redo.logic.helpers :as mh]

            [criterium.core :as cc]))

(def display-char \█)

(def term-width 700)
(def term-height 350)

(def term-font-size 1)

(def standard-mandelbrot-bounds
  (mb/->Bounds 0.3560652830570903 0.3560654830570903
               -0.6445265875444841 -0.6445263875444840))

(def standard-display-bounds
  (mb/->Bounds 0 term-width 0 term-height))

(def point-division-perc 0.05)

(defn color-for [iters]
  (cond
    (>= iters 200) :magenta
    (>= iters 150) :blue
    (>= iters 100) :cyan
    (>= iters 50) :green
    (>= iters 5) :yellow
    :else :red))

(defn screen-coords-in [display-bounds]
  (let [{:keys [min-x max-x min-y max-y]} display-bounds]
    (for [y (range min-y max-y)
          x (range min-x max-x)]
      [x y])))

(defn mandel-screen-coords-in [mandel-bounds display-bounds]
  (->> display-bounds
       (screen-coords-in)
       (map #(vector (mh/map-coord % display-bounds mandel-bounds)
                     %))))

(defn test-coord [coord]
  (let [[x y :as d-coord] (mapv double coord)]
    (mpr/->Point-Result
        d-coord
        (mi/standard-mandelbrot-test-convergence x y))))

(defn point-results-par [perc mandel-bounds display-bounds]
  (let [screen-coords (vec (mandel-screen-coords-in mandel-bounds display-bounds))
        chunk-size (int (* perc (count screen-coords)))]
    (->> screen-coords
         (partition chunk-size)
         (pmap (partial mapv #(update % 0 test-coord)))
         (apply concat)
         (vec))))

(defn point-results-seq [mandel-bounds display-bounds]
  (->> (mandel-screen-coords-in mandel-bounds display-bounds)
       (mapv #(update % 0 test-coord))))

(defn draw-points [term result-points color-f]
  (doseq [[{i :iters} [x y]] result-points]
    (doto term
      (lt/move-cursor x y)
      (lt/set-fg-color (color-f i))
      (lt/put-character display-char))))

(defn -main []
  (let [handler (fn [t-atom _ _] (lt/stop @t-atom))

        t-atom (atom nil)
        term (lt/get-terminal :swing {:cols term-width, :rows term-height,
                                      :font-size term-font-size
                                      :resize-listener (partial handler t-atom)})

        points (time (point-results-par point-division-perc
                                        standard-mandelbrot-bounds
                                        standard-display-bounds))]

    (reset! t-atom term)

    (lt/start term)

    (draw-points term points color-for)))
