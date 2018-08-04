(ns mandelbrot-redo.irrelevant.so-csv
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]))

(def sample-data "qid,question,answer,cSubmodule,correct\n\"11.001\",With respect to flight spoilers they :,\"Option A.     Only operate on the ground.                                                                              \nOption B.     Only operate in flight.                                                                                 \nOption C.     Can operate both on ground and in flight.\",11.9,Option C.     Can operate both on ground and in flight.\n\"11.002\",The prefferd method of Battery charging a Ni-Cad Battery is constant?,\"Option A.     Voltage.                                                                         \nOption B.     Current.                                                                             \nOption C.     Power.\",11.6,Option B.     Current.(CAAIP EEL/1-5 par.4.1)\n\"11.003\",\"In an aircraft control system, employing servo-tabs installation of external ground locks to main control surface :\",\"Option A.     is unnecessary since system is irreversible and therefore control surface cannot be displace by the wind.                                                                                          \nOption B.     would not prevent movement of control column.                                                                                                                           ")

(defn csv-data->maps
  [csv-data]
  (map zipmap
       (->> (first csv-data)
            (map keyword)
            repeat)
       (rest csv-data)))

(doall
  (let [row (first (take 1 (csv-data->maps (csv/read-csv sample-data))))]
    (println "------------")
    (println row)
    (println (get row :qid))
    (println (contains? row :qid))))