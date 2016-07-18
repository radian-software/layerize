(ns layerize.core
  (:require [layerize.equation :as eq]
            [layerize.schematic :as schematic]
            [layerize.problems :as problems]
            [layerize.svg :as svg]
            [layerize.trace :as trace]
            [layerize.util :as util :refer [print! printf! progress]]))

(defn pieces
  "Returns a sequence of SVG-ready cross-section pieces taken
  parallel to the direction-axis and with the specified offset."
  [direction offset]
  (map (fn [path]
         (schematic/inchify
           (schematic/groovify path (zero? direction))))
       (trace/trace
         (eq/cross-section
           (eq/point-mesh 1500 2000)
           direction offset 0.03))))

(defn all-pieces
  "Returns ALL the pieces. With thickness = 0.9, takes about 9-10
  minutes."
  [demo]
  (let [point-mesh (progress "Generating point mesh"
                             (eq/point-mesh 1500 2000))]
    (vec (for [direction [0 1]]
           (let [transverse-direction (- 1 direction)
                 left-limit (->> point-mesh
                              (map #(nth % transverse-direction))
                              (apply min)
                              schematic/next-groove-end
                              (#(- % schematic/half-thickness)))
                 right-limit (->> point-mesh
                               (map #(nth % transverse-direction))
                               (apply max)
                               schematic/prev-groove-start
                               (+ schematic/thickness))
                 _ (println (format "Limits: %f to %f" left-limit right-limit))]
             (vec (for [offset (range left-limit right-limit schematic/twice-thickness)]
                    (progress (format "[%s = %.1f / %s%d / %.2f to %.2f]"
                                      (if (zero? direction) "x" "y")
                                      offset
                                      (if (zero? direction) "X" "Y")
                                      (int (Math/floor (/ (- (+ offset schematic/thickness) left-limit) schematic/twice-thickness)))
                                      (- offset schematic/half-thickness)
                                      (+ offset schematic/half-thickness))
                              (or demo
                                  (vec (map #(schematic/groovify % (zero? direction))
                                            (trace/trace
                                              (eq/cross-section point-mesh direction offset 0.03)))))))))))))

(println (rand-nth ["Hello, I hope you are having a good day."
                    "Hello, you look nice today."
                    "Hello, welcome to Clojure!"]))
