; Copyright © 2015-2016 Radon Rosborough. All rights reserved.
(defproject
  layerize "0.1.0-SNAPSHOT"
  :description "Generates laser-cutter schematics for a certain 3D solid."
  :main layerize.core
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/math.combinatorics "0.1.1"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [quil "2.2.6"]])
