(ns mandelbrot-redo.seesaw-main.helpers
  (:require [mandelbrot-redo.logic.bounds :as mb]
            [seesaw.core :as sc]
            [mandelbrot-redo.logic.async-result :as mar])
  (:import (java.awt Component Container)
           (javax.swing Timer)))

(defn dimensions-of-component [^Component c]
  [(.getWidth c)
   (.getHeight c)])

(defn bounds-from-component [^Component c]
  (->> c
       (dimensions-of-component)
       (mb/from-dimensions)))

(defn new-non-repeatable-runner
  "Returns a function that runs f after call-delay delay.
  Will cancel and reset the delay timer if the returned function is called before completing the previous call.
  Any arguments given to the returned function are applied to the callback."
  [call-delay call-back-f]
  (let [args-atom (atom [])
        t (sc/timer (fn [_] (apply call-back-f @args-atom))
                    :initial-delay call-delay, :repeats? false, :start? false)]

    (fn [& args]
      (reset! args-atom args)
      (.restart ^Timer t))))

(defn parse-double? [n]
  (try
    (Double/parseDouble n)

    (catch NumberFormatException e
      nil)))

(defn force-result-repaint! [canvas results-atom]
  (swap! results-atom mar/invalidate-all)
  (sc/repaint! canvas))

(defn get-root-of [^Container c]
  (if-let [p (.getParent c)]
    (recur p)
    c))