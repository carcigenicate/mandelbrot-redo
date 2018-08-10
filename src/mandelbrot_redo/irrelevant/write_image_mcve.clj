(ns mandelbrot-redo.irrelevant.write-image-mcve
  (:import [java.awt.image BufferedImage]
           (java.util.concurrent Executors Executor)))

(defn lazy-producer [width height]
  (for [y (range height)
        x (range width)]
    [x y (+ x y)]))

; This works fine; finishing after about 5 seconds when width=5000
(defn sync-consumer [results width height]
  (time
    (doseq [[i result] (map vector (range) results)]
      (when (zero? (rem i 1e6))
        (println (str (double (/ i (* width height) 0.01)) "%")))

      ((fn boop [x] x) result)))) ; Data gets consumed here

; This gets to ~30%, then begins being interupted by 1-4 second lags
(defn async-consumer [results width height]
  (doto
    (Thread. ^Runnable
             (fn []
               (sync-consumer results width height)
               (println "Done...")))
    (.start)))

(defn async-consumer2 [results width height]
  (doto
    (Thread. ^Runnable
             (^:once fn* []
               (sync-consumer results width height)
               (println "Done...")))
    (.start)))

(defn async-consumer3 [results width height]
  (doto
    (Thread. ^Runnable
             (^:once fn []
               (sync-consumer results width height)
               (println "Done...")))
    (.start)))

(defn async-consumer4 [results width height]
  (let [d (delay (sync-consumer results width height))]
    (doto
      (Thread. ^Runnable
               (fn []
                 @d
                 (println "Done...")))
      (.start))))

(defn -main []
  (let [width 5000
        height (int (* width 2/3))]
    (-> (lazy-producer width height)
        (async-consumer width height))))