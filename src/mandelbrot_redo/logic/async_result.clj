(ns mandelbrot-redo.logic.async-result)

(defrecord Async-Pack [new-results drawn-results stop-f])

(def new-async-pack
  (->Async-Pack [] [] (constantly nil)))

(defn set-all-drawn [async-pack]
  (assoc async-pack
    :drawn-results (:new-results async-pack)
    :new-results []))

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
        (reset! new-results-atom (:new-results ar))
        (first (get-and-set-new-results ar))))

    @new-results-atom))

(defn set-stop-f [async-pack stop-f]
  (assoc async-pack :stop-f stop-f))

(defn stop-process [async-pack]
  ((:stop-f async-pack)))

