; Copyright © 2015-2016 Radon Rosborough. All rights reserved.
(ns layerize.trace
  (:require [layerize.util :as util :refer [get-x get-y]]))

;; Algorithm config

(def step-size
  0.01)

(def inner-tolerance
  0.01)

(def outer-tolerance
  0.05)

(def removal-tolerance
  0.5)

;; Functions

(defn step-trace
  [{:keys [stage cur-pos path points paths]}]
  (case stage
    :location
    (if (seq points)
      (let [point (apply util/minimize
                         (partial apply +)
                         points)]
        {:stage :begin-tracing
         :cur-pos point
         :path [point]
         :points points
         :paths paths})
      {:paths paths
       :stage :done})
    (:begin-tracing :tracing)
    (let [surrounding
          (filter (partial util/points-within?
                           inner-tolerance outer-tolerance
                           cur-pos)
                  points)
          outward-direction
          (if (#{1} (count (util/group-by-relation
                             (partial util/angles-within? (* 1/4 Math/PI))
                             (map (partial util/point->angle cur-pos)
                                  surrounding))))
            (+ (util/average-angles (map (partial util/point->angle cur-pos)
                                         surrounding))
               Math/PI)
            (- (util/point->angle (nth path
                                       (max 0 (-> path count dec dec)))
                                  cur-pos)
               (* 1/2 Math/PI)))
          target-direction
          (apply util/minimize (partial util/angle-distance-ccw outward-direction)
                 (map (partial util/point->angle cur-pos)
                      surrounding))
          new-pos [(+ (get-x cur-pos) (* step-size (Math/cos target-direction)))
                   (+ (get-y cur-pos) (* step-size (Math/sin target-direction)))]]
      (let [close-to-origin? (util/points-within? inner-tolerance new-pos (first path))
            already-left? (= stage :tracing)]
        (if close-to-origin?
          (if already-left?
            (let [res
                  {:stage :location
                   :points (vec (remove (apply util/any-pred
                                               (map (partial partial util/points-within? removal-tolerance)
                                                    (conj path new-pos)))
                                        points))
                   :paths (conj paths (conj path new-pos))}]
              res)
            {:stage :begin-tracing
             :cur-pos new-pos
             :path (conj path new-pos)
             :points points
             :paths paths})
          {:stage :tracing
           :cur-pos new-pos
           :path (conj path new-pos)
           :points points
           :paths paths})))
    :done
    {:stage :done
     :paths paths}
    ; initialization
    {:stage :location
     :points points
     :paths []}))

(defn trace
  [path]
  (:paths (util/first-where (comp #{:done} :stage)
                            (iterate step-trace
                                     {:points (vec path)}))))
