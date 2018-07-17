(ns mandelbrot-redo.irrelevant.rand-til-zero)

(defn rand-til-zero [k]
  (if (> k 0)
    (recur (doto (rand-int k)
                 (println)))))

#(if(> % 0)(recur(doto(rand-int %)println)))