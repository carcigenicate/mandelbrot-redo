(ns mandelbrot-redo.irrelevant.ballclj)

(defrecord Ball [pos-x pos-y radius vel-x vel-y])

(def canvas-width 500)
(def canvas-height 500)

; Making the atom hold a list to hold multiple balls
(def balls-atom (atom []))

; You should prefer "->Ball" over the "Ball." constructor. The java interop form "Ball." has a few drawbacks
; And I'm adding to the balls vector. What you had before didn't make sense.
;  You put an empty map in the atom, then immedietly overwrote it with a single ball
(swap! balls-atom conj (->Ball 400 405 20 1 1))

; You called this create-ball, but it doesn't create anything. It draws a ball.
(defn draw-ball [ball]
  (println [(:pos-x ball) (:pos-y ball)]))

(defn draw-balls [balls]
  (doseq [ball balls]
    (draw-ball ball)))

; You're mutating atoms here, but that's very poor practice.
; This function should be pure and return the new ball
; I also got rid of the draw call here, since this function has nothing to do with drawing
;  I moved the drawing to animate!.
(defn advance-ball [ball]
  (let [{:keys [pos-x pos-y vel-x vel-y radius]} ball
        ; You had this appearing after the bounds check.
        ; I'd think that you'd want to move, then check the bounds.
        moved-ball (-> ball
                       (update :pos-x + vel-x)
                       (update :pos-y + vel-y))]

    (cond-> moved-ball
      (or (> (+ pos-x radius) canvas-width) (< (- pos-x radius) 0))
      (update :vel-x -) ; This negates the x velocity

      (or (> (+ pos-y radius) canvas-height) (< (- pos-y radius) 60))
      (update :vel-y -))))

; For convenience. Not really necessary, but it helps things thread nicer using ->.
(defn advance-balls [balls]
  (mapv advance-ball balls))

(defn animate! []
  (doseq [_ (range 1000)]
    (swap! balls-atom
      (fn [balls]
        (doto (advance-balls balls) ; doto returns the advanced balls after drawing
          (draw-balls))))

    (Thread/sleep 250)))
