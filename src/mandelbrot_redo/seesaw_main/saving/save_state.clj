(ns mandelbrot-redo.seesaw-main.saving.save-state
  (:require [clojure.string :as s]
            [clojure.edn :as edn]
            [mandelbrot-redo.logic.bounds :as mb]))

(def delimiter-char \&)

(defrecord Save-State [mandel-bounds color-multiples])

(defn serialize [save-state]
  (let [{:keys [mandel-bounds color-multiples]} save-state
        {:keys [min-x max-x min-y max-y]} mandel-bounds
        simple-bounds [min-x max-x min-y max-y]]
    (str simple-bounds delimiter-char color-multiples)))

(defn parse [serialized-save]
  (let [[bounds-str color-multiple-str] (split-with #(not= % delimiter-char)
                                                    serialized-save)]

    (->Save-State (->> bounds-str
                       (apply str)
                       (edn/read-string)
                       (apply mb/->Bounds))

                  (->> color-multiple-str
                       (drop 1) ; Drop the save delimiter
                       (apply str)
                       (edn/read-string)))))
