(ns mandelbrot-redo.logic.concurrent-finder
  (:require [mandelbrot-redo.logic.mandelbrot-iteration :as mi]
            [mandelbrot-redo.logic.point-result :as mpr]
            [mandelbrot-redo.logic.helpers :as mh]
            [mandelbrot-redo.thread-pool :as pool]
            [mandelbrot-redo.logic.bounds :as mb]
            [mandelbrot-redo.logic.async-result :as mar]

            [mandelbrot-redo.irrelevant.lazy-shuffle :as ils]

            [criterium.core :as cc]))

(def pool (pool/new-basic-pool (* 2 (pool/available-processors))))

(defn test-and-record-result [untested-point-result]
  (let [{[r i] :mandel-coord} untested-point-result]
    (mpr/record-test untested-point-result
                     (mi/standard-mandelbrot-test-convergence r i))))

(defn wrap-in-untested-results [mandel-screen-coords]
  (map (partial apply mpr/untested-coord-pair) mandel-screen-coords))

(defn interuptable-chunked-coords [running?-atom division-perc mandel-bounds display-bounds]
  (let [coord-pairs (mh/interuptable-mandel-display-coords-in running?-atom
                                                              mandel-bounds
                                                              display-bounds)
        wrapped-coords (wrap-in-untested-results coord-pairs) ; Shuffle here for nice effect
        chunk-size (int (* division-perc (mb/area display-bounds)))]
    (partition chunk-size wrapped-coords)))

(defn chunked-coords [division-perc mandel-bounds display-bounds]
  (interuptable-chunked-coords (atom true) division-perc mandel-bounds display-bounds))

; ----- Sync methods. Return the result -----

; The "naive" versions are by far the best choice for sync
(defn lazy-naive-point-results-par [division-perc mandel-bounds display-bounds]
    (->> (chunked-coords division-perc mandel-bounds display-bounds)
         (pmap #(mapv test-and-record-result %))))

(defn interuptable-lazy-naive-point-results-par [running?-atom division-perc mandel-bounds display-bounds]
  (->> (interuptable-chunked-coords running?-atom division-perc
                                    mandel-bounds display-bounds)
       (pmap #(mapv test-and-record-result %))))

(defn strict-naive-point-results-par [division-perc mandel-bounds display-bounds]
  (->> (lazy-naive-point-results-par division-perc mandel-bounds display-bounds)
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

; TODO: Cut into subchunks to avoid the repetitive checks?
(defn process-chunk [chunk running?-atom]
  (->> chunk
       (reduce (fn [acc pair]
                 (if @running?-atom
                   (conj! acc (test-and-record-result pair))
                   (reduced (transient []))))
               (transient []))

       (persistent!)))

(defn process-chunk! [chunk result-atom running?-atom]
  (swap! result-atom
         mar/add-results (process-chunk chunk running?-atom)))

(defn pool-async-point-results [result-atom division-perc mandel-bounds display-bounds]
  (let [running?-atom (atom true)
        stop-f (fn [] (reset! running?-atom false)
                      (println "Calculation terminated!"))]

    ; FIXME: Dangerous that the stop-f is set here, then results are added?
    ; FIXME: What if the async-pack is swapped after stop-f is set, but before the results are added?
    (swap! result-atom mar/set-stop-f stop-f)

    (pool/submit-task pool
      ; TODO: chunked-coords is taking awhile. If the operation is attempted to be cancelled
      ; TODO:  before it returns, stop-f wont be valid, and this will continue on.
      (let [chunks (time (interuptable-chunked-coords running?-atom division-perc mandel-bounds display-bounds))]
        (doseq [chunk chunks]
          (pool/submit-task pool
            (process-chunk! chunk result-atom running?-atom)))))

    stop-f))

(defn start-calculating-points! [point-division-perc result-atom mandel-bounds display-bounds]
  ; swap f carries out side effects: The resetting of the atom via stop-process.
  (swap! result-atom
    (fn [result-pack]
      (mar/stop-process result-pack)
      mar/new-async-pack))

  (pool-async-point-results
    result-atom
    point-division-perc
    mandel-bounds
    display-bounds))