(ns mandelbrot-redo.irrelevant.lazy-shuffle)

(defn pop-at [v i]
  [(get v i)
   (vec (concat (subvec v 0 i) (subvec v (inc i))))])

(defn lazy-vec-shuffle [v]
  ((fn rec [remaining]
     (when (seq remaining)
       (let [i (rand-int (count remaining))
             [e rest-elems] (pop-at remaining i)]
         (lazy-seq
           (cons e (rec rest-elems))))))
   v))
