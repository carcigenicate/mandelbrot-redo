(ns mandelbrot-redo.thread-pool
  (:import (java.util.concurrent Executors ExecutorService ScheduledExecutorService ScheduledFuture TimeUnit)))

(defn available-processors []
  (.availableProcessors (Runtime/getRuntime)))

(defn shutdown [^ExecutorService pool]
  (.shutdownNow pool))

(defn- shutdown-with-jvm [^ExecutorService pool]
  (.addShutdownHook (Runtime/getRuntime)
                    (Thread. ^Runnable (fn [] (println "Shutdown Hook Fired")
                                         (shutdown pool)
                                         (println "Pool automatically shut down.")))))

(defn- new-controlled-thread-pool [pool-constrcutor]
  (doto (pool-constrcutor)
        (shutdown-with-jvm)))

; Single-run task pool and methods

(defn new-fixed-pool
  ([n-threads]
   (new-controlled-thread-pool
     #(Executors/newFixedThreadPool n-threads)))

  ([]
   (new-fixed-pool (available-processors))))

(defn submit-task* [^ExecutorService pool, ^Runnable f]
  (.execute pool f))

; TODO: Using "once" to prevent unecessary data retention.
; Apparently "once" is an implementation detail of the language though?
(defmacro submit-task [^ExecutorService pool & body]
  `(submit-task* ~pool (^:once fn* [] ~@body)))

; Sheduled task pool and methods

(defn new-scheduled-thread-pool [n-threads]
  (new-controlled-thread-pool
    #(Executors/newScheduledThreadPool n-threads)))

(defn submit-scheduled*
  "Schedules a task to be run in a sheduled pool.
  Returns a 0-arity function that cancels the task when run."
  [^ScheduledExecutorService pool, milli-rate, ^Runnable f]
  (let [^ScheduledFuture
        fut (.scheduleAtFixedRate pool f 0 milli-rate TimeUnit/MILLISECONDS)]

    (fn [] (.cancel fut true))))

(defmacro submit-scheduled
  "Schedules a task to be run in a sheduled pool.
  Returns a 0-arity function that cancels the task when run."
  [^ScheduledExecutorService pool, milli-rate & body]
  `(submit-scheduled* ~pool ~milli-rate (fn [] ~@body)))



