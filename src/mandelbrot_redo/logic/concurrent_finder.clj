(ns mandelbrot-redo.logic.concurrent-finder
  (:require [mandelbrot-redo.logic.mandelbrot-iteration :as mi]
            [mandelbrot-redo.logic.point-result :as mpr]
            [mandelbrot-redo.logic.helpers :as mh]
            [mandelbrot-redo.thread-pool :as pool]
            [mandelbrot-redo.logic.bounds :as mb]

            [criterium.core :as cc]
            [mandelbrot-redo.logic.async-result :as mar]))

(def pool (pool/new-basic-pool (* 2 (pool/available-processors))))

(defn test-coord [coord]
  (let [[x y :as d-coord] (mapv double coord)]
    (mpr/->Point-Result
      d-coord
      (mi/standard-mandelbrot-test-convergence x y))))

(defn update-mandel-coord-with-result [coord-pair]
  (update coord-pair 0 test-coord))

(defn chunked-coords [division-perc mandel-bounds display-bounds]
  (let [screen-coords (mh/mandel-screen-coords-in mandel-bounds display-bounds)
        chunk-size (int (* division-perc (count screen-coords)))]
    (partition chunk-size screen-coords)))

; ----- Sync methods. Return the result -----

(defn naive-point-results-par [division-perc mandel-bounds display-bounds]
    (->> (chunked-coords division-perc mandel-bounds display-bounds)
         (pmap #(mapv update-mandel-coord-with-result %))
         (apply concat)
         (vec)))

; FAIL
(defn two-pass-future-point-results [division-perc mandel-bounds display-bounds]
    (->> (chunked-coords division-perc mandel-bounds display-bounds)
         (map (fn [chunk]
                (future
                  (mapv update-mandel-coord-with-result chunk))))

         (mapcat deref)

         (vec)))

(defn busy-sync-pool-point-results [division-perc mandel-bounds display-bounds]
    (let [chunks (chunked-coords division-perc mandel-bounds display-bounds)
          result-atom (atom [])
          targ-count (mb/area display-bounds)]
      (doseq [chunk chunks]
        (pool/submit-task pool
          (swap! result-atom
                 into (mapv update-mandel-coord-with-result chunk))))

      ; LOL
      (while (not= targ-count (count @result-atom))
        (Thread/sleep 50))

      @result-atom))

(defn agent-point-results [division-perc mandel-bounds display-bounds]
  (let [chunks (chunked-coords division-perc mandel-bounds display-bounds)
        agents (map agent chunks)]

    (doseq [agt agents]
      (send-off agt (fn [chunk]
                      (mapv update-mandel-coord-with-result chunk))))

    (apply await agents)

    (->> agents
        (map deref)
        (apply concat)
        (vec))))

; ----- Async methods. Immediately return some mutable result object.

(defn pmap-async-point-results [result-atom division-perc mandel-bounds display-bounds]
  (let [chunks (chunked-coords division-perc mandel-bounds display-bounds)

        map-f (fn [chunk]
                (let [chunk-result (mapv update-mandel-coord-with-result chunk)]
                  (swap! result-atom
                         mar/add-results chunk-result)))]

    (pool/submit-task pool
      (->> chunks
           (pmap map-f)
           (vec)))

    nil))

(defn process-chunk [chunk running?-atom]
  (->> chunk
       (reduce (fn [acc pair]
                 (if @running?-atom
                   (conj! acc (update-mandel-coord-with-result pair))
                   (reduced acc)))
               (transient []))

       (persistent!)))

(defn process-chunk! [chunk result-atom running?-atom]
  (swap! result-atom
         mar/add-results (process-chunk chunk running?-atom)))

(defn pool-async-point-results [result-atom division-perc mandel-bounds display-bounds]
  (let [chunks (chunked-coords division-perc mandel-bounds display-bounds)
        running?-atom (atom true)
        stop-f #(reset! running?-atom false)]

    ; FIXME: Dangerous that the stop-f is set here, then results are added?
    ; FIXME: What if the async-pack is swapped after stop-f is set, but before the results are added?
    (swap! result-atom mar/set-stop-f stop-f)

    (doseq [chunk chunks]
      (pool/submit-task pool
        (process-chunk! chunk result-atom running?-atom)))

    stop-f))