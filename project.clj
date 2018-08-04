(defproject mandelbrot-redo "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [seesaw "1.4.5"]
                 [helpers "1"]
                 [criterium "0.4.4"]
                 [clojure-lanterna "0.9.4"]
                 [org.clojure/data.csv "0.1.4"]] ; Remove
  :main ^:skip-aot mandelbrot-redo.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
