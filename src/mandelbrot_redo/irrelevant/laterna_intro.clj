(ns mandelbrot-redo.irrelevant.laterna-intro
  (:require [lanterna.terminal :as lt]))

(def display-char \█)

(defn -main []
  (let [term (lt/get-terminal :swing {:font-size 80,
                                      :palette :windows-xp})]
    (lt/in-terminal term
       (doto term
         (lt/set-fg-color :red)
         (lt/put-string "HELLO WORLD!" 10 10))

      (Thread/sleep 2000))))


