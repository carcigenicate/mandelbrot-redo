(ns mandelbrot-redo.seesaw-main.presets.helpers
  (:require [clojure.string :as s]))

(defn- standardize-name [preset-name]
  (cond
    (string? preset-name) preset-name
    (keyword? preset-name) (name preset-name)
    :else (str preset-name)))

(defn pretty-preset-name [preset-name]
  (let [std-words (-> preset-name
                      (standardize-name)
                      (s/replace \- \space)
                      (s/split #" "))]

    (->> std-words
         (map s/capitalize)
         (s/join " ")
         (apply str))))