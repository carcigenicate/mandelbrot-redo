(ns mandelbrot-redo.irrelevant.scope-counter
  (:require [clojure.edn :as edn]))

(defn parse-code [^String code]
  (edn/read-string (str \[ code \])))

(defn read-structure [^String path]
  (let [code (slurp path)]
    (parse-code code)))

(defn read-structure [^String path]
  (-> path
      (slurp)
      (parse-code)))

(defn let? [coll]
  (and (coll? coll)
       (= (first coll) 'let)))

(defn find-lets [form]
  (let [lets (atom [])
        f (fn rec [elem]
           (when (let? elem)
             (swap! lets conj elem))

           (when (coll? elem)
             (doseq [e elem]
               (rec e))))]

    (f form)

    @lets))

(defn find-lets2 [form]
  (let [f (fn rec [elem]
            (cond
              (let? elem) (concat elem (mapv rec elem))
              (coll? elem) (mapv rec elem)
              :else []))]

    (f form)))


(defn symbols-of-let [let-form]
  (->> let-form
      (second)
      (take-nth 2)))

(defn let-bound-symbols [form]
  (->> form
      (find-lets)
      (mapcat symbols-of-let)))

(def test-code
  '(defn test-f []
     (let [f (fn [a]
               (let [g (fn [b]
                         (let [n 123]
                           (let [m 456])))
                     h (fn [c]
                         (let [z (let [y (let [x (let [k :test]
                                                   k)]
                                           x)]
                                   y)]
                           z))]))
           j (let [i (fn [d])])])))