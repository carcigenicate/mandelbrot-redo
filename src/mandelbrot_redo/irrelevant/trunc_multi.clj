(ns mandelbrot-redo.irrelevant.trunc-multi)

(defn trunc-multi [n m]
  (loop [i n
         j m
         acc [i j]
         neigh {i j}]
    ; (println i j acc neigh)

    (let [k (* i j)
          k-ones (Long/parseLong (str (last (str k))))
          new-acc (conj acc k-ones)]
      (if (= (neigh j) k-ones)
        new-acc
        (recur j k-ones new-acc (assoc neigh j k-ones))))))

(defn trunc-add [n m]
  (loop [i n
         j m
         acc [i j]
         neigh {i j}]
    (let [k (+ i j)
          k-ones (inc (Long/parseLong (str (last (str k)))))
          new-acc (conj acc k-ones)]
      (cond
        (= (neigh j) k-ones) new-acc
        (> (count acc) 100000) :INFINITE
        :else (recur j k-ones new-acc (assoc neigh j k-ones))))))
