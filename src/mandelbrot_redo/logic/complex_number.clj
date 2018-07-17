(ns mandelbrot-redo.logic.complex-number)

(defn square-complex
  ([r i]
   [(- (* r r)
       (* i i))

    (* 2 r i)])

  ([[r i]]
   (square-complex r i)))
