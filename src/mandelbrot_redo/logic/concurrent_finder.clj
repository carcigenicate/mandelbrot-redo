(ns mandelbrot-redo.logic.concurrent-finder
  (:require [mandelbrot-redo.logic.mandelbrot-iteration :as mi]
            [mandelbrot-redo.logic.point-result :as mpr]
            [mandelbrot-redo.logic.helpers :as mh]
            [mandelbrot-redo.thread-pool :as pool]
            [mandelbrot-redo.logic.bounds :as mb]

            [criterium.core :as cc]
            [mandelbrot-redo.logic.async-result :as mar]))

(def pool (pool/new-basic-pool (* 2 (pool/available-processors))))

(defn test-and-record-result [untested-point-result]
  (let [{[r i] :mandel-coord} untested-point-result]
    (mpr/record-test untested-point-result
                     (mi/standard-mandelbrot-test-convergence r i))))

(defn wrap-in-untested-results [mandel-screen-coords]
  (map (partial apply mpr/untested-coord-pair) mandel-screen-coords))

(defn chunked-coords [division-perc mandel-bounds display-bounds]
  (let [coord-pairs (mh/mandel-screen-coords-in mandel-bounds display-bounds)
        wrapped-coords (wrap-in-untested-results coord-pairs)
        chunk-size (int (* division-perc (count wrapped-coords)))]
    (partition chunk-size wrapped-coords)))

; ----- Sync methods. Return the result -----

(defn naive-point-results-par [division-perc mandel-bounds display-bounds]
    (->> (chunked-coords division-perc mandel-bounds display-bounds)
         (pmap #(mapv test-and-record-result %))
         (apply concat)
         (vec)))

; FAIL
(defn two-pass-future-point-results [division-perc mandel-bounds display-bounds]
    (->> (chunked-coords division-perc mandel-bounds display-bounds)
         (map (fn [chunk]
                (future
                  (mapv test-and-record-result chunk))))

         (mapcat deref)

         (vec)))

(defn busy-sync-pool-point-results [division-perc mandel-bounds display-bounds]
    (let [chunks (chunked-coords division-perc mandel-bounds display-bounds)
          result-atom (atom [])
          targ-count (mb/area display-bounds)]
      (doseq [chunk chunks]
        (pool/submit-task pool
                          (swap! result-atom
                                 into (mapv test-and-record-result chunk))))

      ; LOL
      (while (not= targ-count (count @result-atom))
        (Thread/sleep 50))

      @result-atom))

(defn agent-point-results [division-perc mandel-bounds display-bounds]
  (let [chunks (chunked-coords division-perc mandel-bounds display-bounds)
        agents (map agent chunks)]

    (doseq [agt agents]
      (send-off agt (fn [chunk]
                      (mapv test-and-record-result chunk))))

    (apply await agents)

    (->> agents
        (map deref)
        (apply concat)
        (vec))))

; ----- Async methods. Immediately return some mutable result object.

(defn pmap-async-point-results [result-atom division-perc mandel-bounds display-bounds]
  (let [chunks (chunked-coords division-perc mandel-bounds display-bounds)

        map-f (fn [chunk]
                (let [chunk-result (mapv test-and-record-result chunk)]
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
                   (conj! acc (test-and-record-result pair))
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