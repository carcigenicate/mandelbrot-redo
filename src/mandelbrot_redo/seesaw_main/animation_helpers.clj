(ns mandelbrot-redo.seesaw-main.animation-helpers)

(defrecord Animation-Range [min-n max-n step current])

(defn new-range [min-n max-n step]
  (->Animation-Range min-n max-n step min-n))

(defn from-steps [min-n max-n steps]
  (mapv #(new-range min-n max-n %) steps))

(defn advance
  "Advances the current value of the animation range.
  Bounds are both inclusive."
  [anim-range]
  (let [{:keys [min-n max-n step current]} anim-range
        new-val (+ current step)]

    (if (<= min-n new-val max-n)
      (assoc anim-range :current new-val)

      (-> anim-range
          (assoc :current (if (< new-val min-n)
                            (+ min-n (- min-n new-val))
                            (- max-n (- new-val max-n))))
          (update :step -)))))

; -----

