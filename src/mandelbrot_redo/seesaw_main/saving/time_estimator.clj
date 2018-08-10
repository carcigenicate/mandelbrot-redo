(ns mandelbrot-redo.seesaw-main.saving.time-estimator
  (:require [helpers.general-helpers :as g]))

(defrecord Snapshot [ms-timestamp progress])
(defrecord Estimator [snapshots n-snapshots-to-keep])

(defn new-estimator [n-snapshots-to-keep]
  (->Estimator [] n-snapshots-to-keep))

(defn- trim-old [estimator]
  (let [{:keys [snapshots n-snapshots-to-keep]} estimator
        n-excess (- n-snapshots-to-keep (count snapshots))]

    (if (neg? n-excess)
      (update estimator :snapshots subvec (- n-excess))
      estimator)))

(defn add-snapshot
  ([estimator ms-timestamp progress]
   (-> estimator
       (update :snapshots conj (->Snapshot ms-timestamp progress))
       (trim-old)))

  ([estimator progress]
   (add-snapshot estimator (g/current-ms-timestamp) progress)))
#_
(defn average-ms-per-progress [estimator]
  (let [{:keys [snapshots]} estimator
        ms-per-progress-sum (->> snapshots
                                 (map (fn [{:keys [ms-timestamp progress]}]
                                        (/ ms-timestamp progress)))
                                 (apply +))]

    (double (/ ms-per-progress-sum (count snapshots)))))
#_
(defn average-ms-per-progress [estimator]
  (let [{:keys [snapshots]} estimator]
    (loop [[snapshot & rest-snapshots] snapshots
           last-snapshot
           ms-per-progress-samples []]
      (cond
        (nil? snapshot)
        (let [sum (apply + ms-per-progress-samples)]
          (double (/ sum ())))))))

(defn- abs [n]
  (if (neg? n) (- n) n))


(defn ms-per-progress [snapshot1 snapshot2]
  (let [{t1 :ms-timestamp, p1 :progress} snapshot1
        {t2 :ms-timestamp, p2 :progress} snapshot2
        prog-diff (abs (- p1 p2))]
    (if (zero? prog-diff)
      prog-diff

      (double
        (/ (abs (- t1 t2))
           prog-diff)))))

(defn average-ms-per-progress [estimator]
  (let [{:keys [snapshots]} estimator]
    (if (> (count snapshots) 1)
      (let [sum (->> snapshots
                     (partition 2 1)
                     (reduce (fn [ms-per-prog-sum snapshot-pair]
                               (->> snapshot-pair
                                    (apply ms-per-progress)
                                    (+ ms-per-prog-sum)))
                             0))]
        (double (/ sum
                   (dec (count snapshots)))))

      Double/POSITIVE_INFINITY)))

(defn estimated-ms-remaining [estimator current-progress max-progress]
  (let [avg-ms (average-ms-per-progress estimator)
        prog-remaining (- max-progress current-progress)]

    (* avg-ms prog-remaining)))


(let [second 1000
      minute (* second 60)
      hour (* minute 60)
      day (* hour 24)]
  (defn format-ms [ms precision]
    (let [form #(format (str "%."precision"f") (double %))
          div #(form (/ ms %))]
      (cond
        (Double/isInfinite ms) (str "INF" #_\∞)
        (< ms second) (str (form ms) " ms") ; Less than a second, show ms
        (< ms minute) (str (div second) " secs") ; Less than an minute, show seconds
        (< ms hour) (str (div minute) " mins") ; Less than a hour, show minutes
        (< ms day) (str (div hour) " hours") ; Less than a day, show hours
        :else (str (div day)))))) ; else show days

