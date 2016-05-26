; Copyright © 2015-2016 Radon Rosborough. All rights reserved.
(ns layerize.bezier
  (:require [clojure.string :as string]
            [layerize.svg :refer [point->str]]
            [layerize.util :refer [get-x get-y]]))

(defn GetCurveControlPoints
  "Returns a vector with a list of the first
  control points and a list of the second control
  points, both of length one less than that of
  knots (the points through which the cubic Bézier
  polyspline should pass). Adapted from the C# code
  available at http://www.codeproject.com/Articles/31859/Draw."
  [knots]
  (letfn [(GetFirstControlPoints
            [rhs]
            (let [n (count rhs)
                  [x tmp _]
                  (reduce
                    (fn [[x tmp b] i]
                      (let [tmp (assoc tmp i (/ b))
                            b (- (if (< i (dec n)) 4.0 3.5) (nth tmp i))
                            x (assoc x i (/ (- (nth rhs i) (nth x (dec i))) b))]
                        [x tmp b]))
                    (let [b 2.0]
                      [(vec (cons (/ (nth rhs 0) b) (repeat (dec n) 0)))
                       (vec (repeat n 0))
                       b])
                    (range 1 n))]
              (reduce
                (fn [x i]
                  (assoc x (- n i 1) (- (nth x (- n i 1))
                                        (* (nth tmp (- n i)) (nth x (- n i))))))
                x
                (range 1 n))))]
    (let [n (dec (count knots))]
      (if (= n 1)
        (let [first-control-points
              [[(/ (+ (* 2 (get-x (nth knots 0)))
                      (get-x (nth knots 1)))
                   3)
                (/ (+ (* 2 (get-y (nth knots 0)))
                      (get-y (nth knots 1)))
                   3)]]
              second-control-points
              [[(- (* 2 (get-x (nth first-control-points 0)))
                   (get-x (nth knots 0)))
                (- (* 2 (get-y (nth first-control-points 0)))
                   (get-y (nth knots 0)))]]]
          [first-control-points second-control-points])
        (let [[x y]
              (for [get-c [get-x get-y]]
                (GetFirstControlPoints
                  (conj
                    (vec (cons
                           (+ (get-c (nth knots 0))
                              (* 2 (get-c (nth knots 1))))
                           (for [i (range 1 (dec n))]
                             (+ (* 4 (get-c (nth knots i)))
                                (* 2 (get-c (nth knots (inc i))))))))
                    (/ (+ (* 8 (get-c (nth knots (dec n))))
                          (get-c (nth knots n)))
                       2.0))))]
          [(for [i (range n)]
             [(nth x i) (nth y i)])
           (for [i (range n)]
             (if (< i (dec n))
               [(- (* 2 (get-x (nth knots (inc i))))
                   (nth x (inc i)))
                (- (* 2 (get-y (nth knots (inc i))))
                   (nth y (inc i)))]
               [(/ (+ (get-x (nth knots n))
                      (nth x (dec n)))
                   2)
                (/ (+ (get-y (nth knots n))
                      (nth y (dec n)))
                   2)]))])))))

(defn spline
  [start first-control second-control end]
  ["path" {"d" (string/join " "
                            [(str "M" (point->str start))
                             (str "C" (point->str first-control))
                             (point->str second-control)
                             (point->str end)])}])

(defn polyspline
  ([points first-controls second-controls]
   (for [[start first-control second-control end]
         (map vector
              points
              first-controls
              second-controls
              (rest points))]
     (spline start first-control second-control end)))
  ([points]
   (apply polyspline points (GetCurveControlPoints points))))
