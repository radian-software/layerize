; Copyright © 2015-2016 Radon Rosborough. All rights reserved.
(ns layerize.util
  (:require [clojure.math.numeric-tower :refer [expt]]))

(set! *warn-on-reflection* true)

; should add pre and post macros here

(defn print!
  [& more]
  (do (apply clojure.core/print more) (flush)))

(defn printf!
  [fmt & args]
  (do (apply clojure.core/printf fmt args) (flush)))

(defmacro progress
  [pre-msg expr]
  `(do
     (printf! "%s... " ~pre-msg)
     (let [start# (System/nanoTime)
           result# ~expr]
       (printf! "done in %.1f seconds.%n" (/ (- (System/nanoTime) start#) 1000000000.0))
       result#)))

(defn queue
  ([] clojure.lang.PersistentQueue/EMPTY)
  ([& coll] (reduce conj clojure.lang.PersistentQueue/EMPTY coll)))

(defmethod print-method clojure.lang.PersistentQueue
  [q ^java.io.Writer w]
  (.write w "#queue ")
  (print-method (sequence q) w))

(defn xor
  ([] nil)
  ([a] a)
  ([a b] (if a (if b nil a) (if b b nil)))
  ([a b & more] (xor (xor a b) (apply xor more))))

(defn exact-add
  [a b places]
  (double (/ (+ (Math/round ^Double (* a (expt 10 places)))
                (Math/round ^Double (* b (expt 10 places))))
             (expt 10 places))))

(defn exact-subtract
  [a b places]
  (double (/ (- (Math/round ^Double (* a (expt 10 places)))
                (Math/round ^Double (* b (expt 10 places))))
             (expt 10 places))))

(defn exact-range
  [start end step places]
  (lazy-seq
    (if (< start end)
      (cons start (exact-range (exact-add start step places) end step places)))))

(defn mapcat-indexed
  "mapcat crossed with map-indexed"
  [f & colls]
  (apply concat (apply map-indexed f colls)))

(defn average
  "Returns the mean value of the xs."
  [& xs]
  (/ (apply + xs) (count xs)))

(defn first-where
  "Returns the first element in coll for which pred
  returns logical true, else nil."
  [pred coll]
  (first (filter pred coll)))

(defn first-index-where
  "Returns the index of the first element in coll
  for which pred returns logical true, else nil."
  [pred coll]
  (first (keep-indexed #(when (pred %2) %1) coll)))

(defn count-range
  "Returns a range containing n+1 equally spaced
  elements, starting at start and ending at stop."
  [start stop n]
  (let [step (/ (- stop start)
                n)]
    (range start
           (+ stop
              (/ step 2))
           step)))

(defn rotate
  "Returns a vector of the elements after index amt,
  inclusive, followed by the remaining elements."
  [coll amt]
  (reduce conj
          (vec (drop amt coll))
          (take amt coll)))

(defn drop-every-nth
  "Returns a seq containing the elements of coll except
  for every nth element, where start (defaults to n-1) is
  the first index dropped."
  ([coll n]
   (map second
        (remove #(-> % first inc (mod n) zero?)
                (map-indexed vector
                             coll))))
  ([coll n start]
   (map second
        (remove #(-> % first (- start) (mod n) zero?)
                (map-indexed vector
                             coll)))))

(defn drop-nth
  "Returns a vector (if coll is a vector) or lazy
  seq containing all of coll except the nth element."
  [coll n]
  (if (vector? coll)
    (vec (concat (subvec coll 0 n)
                 (subvec coll (inc n) (count coll))))
    (concat (take n coll)
            (drop (inc n) coll))))

(defn take-while+
  "Like take-while but returns the first element for
  which pred is false."
  [pred coll]
  (lazy-seq
    (when-let [s (seq coll)]
      (if (pred (first s))
        (cons (first s)
              (take-while+ pred (rest s)))
        (list (first s))))))

(defn any-pred
  "Returns a function that returns true if any of its
  composing predicates return a logical true value on
  its argument and otherwise returns false."
  [& preds]
  (fn [x]
    (boolean (some #(% x) preds))))

(defn minimize
  "Returns the item that minimizes the value of f."
  ([f x] x)
  ([f x y] (if (< (f x) (f y)) x y))
  ([f x y & more] (reduce (partial minimize f) (minimize f x y) more)))

(defn maximize
  "Returns the item that maximizes the value of f."
  ([f x] x)
  ([f x y] (if > (f x) (f y)) x y)
  ([f x y & more] (reduce (partial maximize f) (maximize f x y) more)))

(defn depair
  "(depair (partition 2 1 coll)) => coll"
  [pairs]
  (if (seq pairs)
    (conj (vec (map first pairs)) (second (last pairs)))))

(defn group-by-relation
  "Groups coll using the binary predicate rel to
  determine whether two elements are in the same
  group. After processing, every element will
  compare logical true to at least one other
  element in its group, and no element will compare
  logical true to any element in another group."
  [rel coll]
  (loop [coll coll
         groups []]
    (if (seq coll)
      (let [{:keys [coll group]}
            (loop [new-items [(first coll)]
                   coll (rest coll)
                   group []]
              (if (seq new-items)
                (if (seq coll)
                  (let [extracted (group-by (partial rel (first new-items))
                                            coll)]
                    (recur (concat (rest new-items) (get extracted true))
                           (get extracted false)
                           (conj group (first new-items))))
                  {:coll nil
                   :group (concat group new-items)})
                {:coll coll
                 :group group}))]
        (recur coll
               (conj groups group)))
      groups)))

(defn narrow
  ([f det lo guess hi]
   (let [res (f guess)
         com (det res)]
     (cond
       (zero? com) res
       (pos? com) (recur f det
                         guess
                         (* 1/2 (+ guess hi))
                         hi)
       :neg?-com (recur f det
                        lo
                        (* 1/2 (+ lo guess))
                        guess))))
  ([f det lo guess hi limit]
   (if (zero? limit)
     guess
     (let [res (f guess)
           com (det res)]
       (cond
         (zero? com) res
         (pos? com) (recur f det
                           guess
                           (* 1/2 (+ guess hi))
                           hi
                           (dec limit))
         :neg?-com (recur f det
                          lo
                          (* 1/2 (+ lo guess))
                          guess
                          (dec limit)))))))

(defn group-into
  ([within? coll n lo guess hi]
   (narrow (fn [tolerance]
             (group-by-relation (partial within? tolerance)
                                coll))
          #(- (count %) n)
          lo
          guess
          hi))
  ([within? coll n lo guess hi limit]
   (narrow (fn [tolerance]
             (group-by-relation (partial within? tolerance)
                                coll))
           #(- (count %) n)
           lo
           guess
           hi
           limit)))

(defn get-x
  "Returns the x-coordinate of the point."
  [point] (first point))
(defn get-y
  "Returns the y-coordinate of the point."
  [point] (second point))
(defn get-ul
  "Returns the upper-left coordinate of the bounding box."
  [box] (first box))
(defn get-lr
  "Returns the lower-right coordinate of the bounding box."
  [box] (second box))

(defn bounds
  "Returns the smallest bounding box that contains all the points."
  [points]
  (case (count points)
    0 [[0 0]
       [1 1]]
    1 [[(dec (get-x (first points)))
        (inc (get-x (first points)))]
       [(dec (get-y (first points)))
        (inc (get-y (first points)))]]
    [[(apply min (map get-x points))
      (apply min (map get-y points))]
     [(apply max (map get-x points))
      (apply max (map get-y points))]]))

(defn rescale
  "Linearly rescales the points from the first bounding box to
  the second."
  [points [[left0 top0] [right0 bottom0]] [[left1 top1] [right1 bottom1]]]
  (map (fn [[x y]]
         [(-> x
            (- left0)
            (/ (- right0 left0))
            (* (- right1 left1))
            (+ left1))
          (-> y
            (- top0)
            (/ (- bottom0 top0))
            (* (- bottom1 top1))
            (+ top1))])
       points))

(defn squarify
  "Rescales the second bounding box so that a rescale from the
  first to the second will not change the aspect ratio. The second
  bounding box is always made smaller rather than larger."
  ([[[left0 top0] [right0 bottom0]] [[left1 top1] [right1 bottom1]] [buffer-x buffer-y]]
   (let [src-width (- right0 left0)
         src-height (- bottom0 top0)
         src-aspect (/ src-width src-height)
         dst-width (- right1 left1)
         dst-height (- bottom1 top1)
         dst-aspect (/ dst-width dst-height)
         aspect-ratio (/ dst-aspect src-aspect)
         [adj-dst-width adj-dst-height] (if (> aspect-ratio 1)
                                          [(/ dst-width aspect-ratio)
                                           dst-height]
                                          [dst-width
                                           (* dst-height aspect-ratio)])
         x-buffer (+ (* 1/2 (- dst-width adj-dst-width))
                     buffer-x)
         y-buffer (+ (* 1/2 (- dst-height adj-dst-height))
                     buffer-y)]
     [[(+ left1 x-buffer)
       (+ top1 y-buffer)]
      [(- right1 x-buffer)
       (- bottom1 y-buffer)]]))
  ([[[left0 top0] [right0 bottom0]] [[left1 top1] [right1 bottom1]]]
   (squarify [[left0 top0] [right0 bottom0]] [[left1 top1] [right1 bottom1]] [0 0])))

(defn compose
  "Returns a new bounding box such that the following
  two commands will perform the same task:

  (-> points (rescale box0 box1) (rescale box2 box3))
  (-> points (rescale box0 (compose box1 box2 box3)))"
  [[[left1 top1] [right1 bottom1]]
   [[left2 top2] [right2 bottom2]]
   [[left3 top3] [right3 bottom3]]]
  (let [right0-left0 (* (- right1 left1)
                        (- right3 left3)
                        (/ (- right2 left2)))
        left0 (+ (* (- left1 left2)
                    (- right3 left3)
                    (/ (- right2 left2)))
                 left3)
        right0 (+ left0 right0-left0)

        bottom0-top0 (* (- bottom1 top1)
                        (- bottom3 top3)
                        (/ (- bottom2 top2)))
        top0 (+ (* (- bottom1 top1)
                   (- bottom3 top3)
                   (/ (- bottom2 top2)))
                left3)
        bottom0 (+ top0 bottom0-top0)]
    [[left0 top0] [right0 bottom0]]))

(defn angle-distance-ccw
  "Returns the counterclockwise distance between
  the angles x and y in radians (between 0 and 2π)."
  [x y]
  (mod (- y x) (* 2 Math/PI)))

(defn angle-distance-cw
  "Returns the clockwise distance between the angles
  x and y in radians (between 0 and 2π)."
  [x y]
  (mod (- x y) (* 2 Math/PI)))

(defn angle-distance
  "Returns the shortest angular distance between the
  angles x and y in radians (between 0 and π)."
  [x y]
  (let [d (* (- x y))]
    (min (mod d (* 2 Math/PI))
         (mod (- d) (* 2 Math/PI)))))

(defn angle-distance
  "Returns the distance between the angles x and
  y in radians (between 0 and π)."
  [x y]
  (let [x (mod x (* 2 Math/PI))
        y (mod y (* 2 Math/PI))]
    (min (Math/abs ^Double (- y x))
         (Math/abs ^Double (- y (* 2 Math/PI) x)))))

(defn angles-within?
  "Returns true if the distance between the angles
  x and y in radians is less than tolerance, else
  false."
  [tolerance x y]
  (< (angle-distance x y)
     tolerance))

(defn group-angles
  "Groups a list of angles (in radians) into n groups,
  each containing angles close to one another. Accounts
  for 2π->0 wraparound."
  [angles n]
  (narrow (fn [tolerance]
            (group-by-relation
              (partial angles-within? tolerance)
              angles))
          #(- % n)
          0.0
          (* 2 Math/PI)
          (* 1/4 Math/PI)))

(defn strictly-between?
  "Returns true if x is between left and right, exclusive, and
  false otherwise."
  [left right x]
  (and (> x left)
       (< x right)))

(defn between?
  "Returns true if x is between left and right, inclusive, and
  false otherwise."
  [left right x]
  (and (>= x left)
       (<= x right)))

(defn points-within?
  "Returns true if the Euclidean distance between the
  2D points p and q is less than outer and greater than
  inner (defaults to 0), else false."
  ([outer p q]
   (let [x (- (first q) (first p))
         y (- (second q) (second p))]
     (< (+ (* x x)
           (* y y))
        (* outer outer))))
  ([inner outer p q]
   (and (points-within? outer p q)
        (not (points-within? inner p q)))))

(defn points-without?
  "Returns true if the Euclidean distance between the
  2D points p and q is greater than outer or less than
  inner (defaults to 0), else false."
  ([outer p q]
   (let [x (- (first q) (first p))
         y (- (second q) (second p))]
     (> (+ (* x x)
           (* y y))
        (* outer outer))))
  ([inner outer p q]
   (and (points-without? outer p q)
        (not (points-without? inner p q)))))

(defn average-angles
  "Finds the average value of a collection of angles,
  using a vector sum."
  [angles]
  (Math/atan2 (apply +
                     (map #(Math/sin %)
                          angles))
              (apply +
                     (map #(Math/cos %)
                          angles))))

(defn point->angle
  "Finds the direction from point p (defaults to the
  origin) to point q."
  ([p q]
   (point->angle (map - q p)))
  ([q]
   (Math/atan2 (get-y q) (get-x q))))
