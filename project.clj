(defproject layerize "0.1.0-SNAPSHOT"
  :description "Generates laser-cutter schematics for a certain 3D solid."
  :url "https://github.com/raxod502/layerize"

  :license {:name "MIT License"
            :url "http://www.opensource.org/licenses/mit-license.php"}

  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/math.combinatorics "0.1.1"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [quil "2.2.6"]]

  :main layerize.core)
