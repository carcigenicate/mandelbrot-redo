(ns mandelbrot-redo.seesaw-main.key-handler
  (:require [mandelbrot-redo.logic.bounds :as mb])
  (:import (java.awt.event KeyEvent)))

(def arrow-key-code->keyword
  {KeyEvent/VK_LEFT :left
   KeyEvent/VK_RIGHT :right
   KeyEvent/VK_UP :up
   KeyEvent/VK_DOWN :down})

(defn map-code-to-char [key-code]
  (or (arrow-key-code->keyword key-code)
      (Character/toLowerCase (char key-code))))

(defn alter-bounds-with-key? [mandel-bounds key-code]
  (let [b mandel-bounds

        shift #(mb/shift-by b % %2)
        zoom #(mb/adjust-size b %)

        [hw hh] (mapv #(double (/ % 2)) (mb/dimensions b))
        zoom-by (min hw hh)]

    (case (map-code-to-char key-code)
      (:left \a) (shift (- hw) 0)
      (:right \d) (shift hw 0)
      (:up \w) (shift 0 (- hh))
      (:down \s) (shift 0 hh)

      \z (zoom (* 0.5 zoom-by))
      \x (zoom (- zoom-by))

      nil)))