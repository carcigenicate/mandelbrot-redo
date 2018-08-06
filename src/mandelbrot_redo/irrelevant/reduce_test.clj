(ns mandelbrot-redo.irrelevant.reduce-test)

(defn reduce-test []
  (reduce (fn [acc n]
            (println "This should print. Right?" n)
            (if (odd? n)
              (reduced n)
              acc))
          nil
          (range 10)))
