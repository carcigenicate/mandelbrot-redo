(ns mandelbrot-redo.logic.async-result)

(defrecord Async-Pack [new-results drawn-results stop-f])

(def new-async-pack
  (->Async-Pack [] [] (constantly nil)))

(defn set-all-drawn [async-pack]
  (-> async-pack
      (assoc :new-results [])
      (update :drawn-results into (:new-results async-pack))))


(defn add-results [async-pack results]
  (update async-pack :new-results conj results))

(defn get-and-set-new-results
  "Returns a pair of [new-async-pack new-results]"
  [async-pack]
  [(set-all-drawn async-pack)
   (:new-results async-pack)])

(defn get-and-set-new-results!
  "Returns the newly-added results, and mutates the atom using set-all-drawn."
  [async-pack-atom]
  (let [new-results-atom (atom [])]
    (swap! async-pack-atom
      (fn [ar]
        (let [[new-ar results] (get-and-set-new-results ar)]
          (reset! new-results-atom results)
          new-ar)))

    @new-results-atom))

(defn mark-and-get-all! [async-pack-atom]
  (let [results-atom (atom [])]
    (swap! async-pack-atom
      (fn [ar]
        ; TODO: Figure out which result set is bigger, and add the smaller to the larger
        (reset! results-atom (into (:new-results ar) (:drawn-results ar)))

        (set-all-drawn ar)))

    @results-atom))



(defn set-stop-f [async-pack stop-f]
  (assoc async-pack :stop-f stop-f))

(defn stop-process [async-pack]
  ((:stop-f async-pack)))

(defn invalidate-all [async-pack]
  (assoc async-pack :new-results (:drawn-results async-pack)
                    :drawn-results []))

