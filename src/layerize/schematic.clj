; Copyright © 2015-2016 Radon Rosborough. All rights reserved.
(ns layerize.schematic
  (:require [layerize.equation :as eq]
            [layerize.trace :as trace]
            [layerize.util :as util :refer [get-x get-y between?]]))

(set! *warn-on-reflection* true)

; thickness 0.3 => ~100 pieces
; thickness 0.9 => ~30 pieces
(def thickness 0.3)
(def half-thickness (/ thickness 2))
(def twice-thickness (* thickness 2))
(def material-thickness 0.125)

(defn prev-groove-start
  "Assuming that thickness == 0.10,
  [-0.05, 0.15) -> -0.05
  [0.15, 0.35) -> 0.15
  etc."
  [x]
  (- (* (Math/floor (/ (+ x half-thickness)
                       twice-thickness))
        twice-thickness)
     half-thickness))

(defn next-groove-start
  "Assuming that thickness == 0.10,
  (-0.05, 0.15] -> 0.15
  (0.15, 0.35] -> 0.35
  etc."
  [x]
  (- (* (Math/ceil (/ (+ x half-thickness)
                      twice-thickness))
        twice-thickness)
     half-thickness))

(defn prev-groove-end
  "Assuming that thickness == 0.10,
  [-0.15, 0.05) -> -0.15
  [0.05, 0.25) -> 0.05
  etc."
  [x]
  (+ (* (Math/floor (/ (- x half-thickness)
                       twice-thickness))
        twice-thickness)
     half-thickness))

(defn next-groove-end
  "Assuming that thickness == 0.10,
  (-0.15, 0.05] -> 0.05
  (0.05, 0.25] -> 0.25
  etc."
  [x]
  (+ (* (Math/ceil (/ (- x half-thickness)
                      twice-thickness))
        twice-thickness)
     half-thickness))

(defn interpolate-x
  "Returns the point with the given x-coordinate lying on the
  line connecting p1 and p2."
  [x p1 p2]
  [x (+ (get-y p1)
        (* (/ (- (get-y p2)
                 (get-y p1))
              (- (get-x p2)
                 (get-x p1)))
           (- x
              (get-x p1))))])

(defn interpolate-y
  "Returns the point with the given y-coordinate lying on the
  line connecting p1 and p2."
  [y p1 p2]
  [(+ (get-x p1)
      (* (/ (- (get-x p2)
               (get-x p1))
            (- (get-y p2)
               (get-y p1)))
         (- y
            (get-y p1)))) y])

(defn crossings
  [path left right]
  (let [within? (partial between? left right)
        first-outside-index (inc (count (take-while within?
                                                    (map get-x path))))
        shifted-path (reduce conj (vec (drop first-outside-index path))
                             (take first-outside-index path))
        initial-state (if (< (-> shifted-path first get-x)
                             left)
                        :on-left :on-right)
        puncture-state (let [rtlf (group-by #(> (get-x %) (util/average left right))
                                            path)] ; right = true, left = false
                         (if (> (count (get rtlf true))
                                (count (get rtlf false)))
                           :from-right ; allow punctures from the right
                           :from-left))
        _ (println "puncture state is" puncture-state)] ; allow punctures from the left
    (loop [path shifted-path
           state initial-state
           crossings []
           cur-crossing nil]
      (if path
        (case state
          :on-left (if (>= (-> path first get-x)
                           left)
                     (recur (next path)
                            :from-left
                            crossings
                            [(first path)])
                     (recur (next path)
                            :on-left
                            crossings
                            nil))
          :on-right (if (<= (-> path first get-x)
                            right)
                      (recur (next path)
                             :from-right
                             crossings
                             [(first path)])
                      (recur (next path)
                             :on-right
                             crossings
                             nil))
          :from-left (cond
                       (> (-> path first get-x)
                          right)
                       (recur (next path)
                              (do (println "passing from left to right: crossing of length" (count cur-crossing)) :on-right)
                              (conj crossings cur-crossing)
                              nil)
                       (< (-> path first get-x)
                          left)
                       (recur (next path)
                              :on-left
                              (if (= state puncture-state)
                                (reduce conj crossings [cur-crossing cur-crossing])
                                crossings)
                              nil)
                       :otherwise
                       (recur (next path)
                              :from-left
                              crossings
                              (conj cur-crossing (first path))))
          :from-right (cond
                        (< (-> path first get-x)
                           left)
                        (recur (next path)
                               (do (println "passing from right to left: crossing of length" (count cur-crossing)) :on-left)
                               (conj crossings cur-crossing)
                               nil)
                        (> (-> path first get-x)
                           right)
                        (recur (next path)
                               :on-right
                               (if (= state puncture-state)
                                 (reduce conj crossings [cur-crossing cur-crossing])
                                 crossings)
                               nil)
                        :otherwise
                        (recur (next path)
                               :from-right
                               crossings
                               (conj cur-crossing (first path)))))
        crossings))))

(defn bridge
  [path initial-seg final-seg]
  (vec (util/take-while+ (partial not= (peek final-seg))
                         (drop-while (partial not= (first initial-seg))
                                     (cycle path)))))

(defn midpoint-heights
  [path crossings left right bottom? license-to-kill?]
  (if license-to-kill? (println "[LICENSE TO KILL]"))
  (->> crossings ; [[[x y] & more] & more]
    (map (fn [crossing]
           [crossing
            [(apply min (map get-y crossing))
             (apply max (map get-y crossing))]])) ; [[[[x y] & more] [min max]] & more]
    (sort-by (comp first second)) ; sort by min
    (partition 2) ; [[[[[x y] & more] [min max]] [[[x y] & more] [min max]]] & more]
    (map (fn [[[lower-crossing [lower-min lower-max]]
               [upper-crossing [upper-min upper-max]]]]
           (println "[map]" (count lower-crossing) (count upper-crossing))
           (if (not= lower-crossing upper-crossing)
             (if (pos? (- upper-min lower-max))
               (let [crossing (if bottom? lower-crossing upper-crossing)
                     midpoint-height (util/average lower-max upper-min)]
                 (if (empty? (filter (fn [[x y]]
                                       (and (>= x left)
                                            (<= x right)
                                            (> y lower-max)
                                            (< y upper-min)))
                                     path))
                   (do
                     (println "Creating normal groove.")
                     [crossing
                      :normal
                      midpoint-height])
                   (do
                     (println "Cutting off corner with groove.")
                     (when license-to-kill?
                       (let [resume-point
                             (last (take-while #(if bottom?
                                                  (<= (get-y %) midpoint-height)
                                                  (>= (get-y %) midpoint-height))
                                               (drop-while (partial not= (last crossing))
                                                           (cycle path))))]
                         (println "midpoint-height =" midpoint-height "/ resume-point =" resume-point)
                         [(util/minimize count
                                         (bridge path crossing [resume-point])
                                         (bridge path [resume-point] crossing))
                          :kill-L
                          midpoint-height])))))
               (when license-to-kill?
                 (println "Cutting off point with groove.")
                 [(util/minimize count
                                 (bridge path lower-crossing upper-crossing)
                                 (bridge path upper-crossing lower-crossing))
                  :kill-vertical
                  nil]))
             (when license-to-kill?
               (println "Flattening edge with groove.")
               [lower-crossing :kill-vertical nil])))) ; [[[x y] & more] avg mode] or nil
    (filter identity))) ; [[[x y] & more] avg]

(defn splice-list
  [path splicing index length]
  (let [L (count path)
        l length
        i index]
    (vec (concat (->> path
                   (take i)
                   (drop (- l (- L i))))
                 splicing
                 (drop (+ i l) path)))))

(defn surround?
  "Returns true if x is between bound1 and bound2,
  regardless of whether bound2 is larger than bound1."
  [bound1 bound2 x]
  (or (and (<= bound1 x)
           (< x bound2))
      (and (<= bound2 x)
           (< x bound1))
      (and (< bound1 x)
           (<= x bound2))
      (and (< bound2 x)
           (<= x bound1))))

(defn splice
  [path [crossing mode midpoint-height] left right]
  (println "Splicing at" midpoint-height "for left/right =" left right "and crossing length" (count crossing))
  (let [start-index (count (take-while (partial not= (first crossing))
                                       path))
        end-index (count (take-while (partial not= (last crossing))
                                     path))
        splice-start (let [before (nth path (dec start-index))
                           during (nth path start-index)]
                       (println "before/during for start =" before during)
                       (cond
                         (surround? (get-x before) (get-x during) left)
                         (interpolate-x left before during)
                         (surround? (get-x before) (get-x during) right)
                         (interpolate-x right before during)
                         (surround? (get-y before) (get-y during) midpoint-height)
                         (interpolate-y midpoint-height before during)
                         :otherwise (throw (Exception. "crossing did not end at groove boundary"))))
        splice-end (let [before (nth path end-index)
                         during (nth path (inc end-index))]
                     (println "before/during for end =" before during)
                     (cond
                       (surround? (get-x before) (get-x during) left)
                       (interpolate-x left before during)
                       (surround? (get-x before) (get-x during) right)
                       (interpolate-x right before during)
                       (surround? (get-y before) (get-y during) midpoint-height)
                       (interpolate-y midpoint-height before during)
                       :otherwise (throw (Exception. "crossing did not end at groove boundary"))))
        _ (println "Splice is" splice-start "->" splice-end)
        splicing (case mode
                   :normal
                   [splice-start
                    [(get-x splice-start) midpoint-height]
                    [(get-x splice-end) midpoint-height]
                    splice-end]
                   :kill-vertical
                   [splice-start
                    splice-end]
                   :kill-L
                   (cond
                     (= (get-y splice-start) midpoint-height)
                     [splice-start
                      [(get-x splice-end) (get-y splice-start)]
                      splice-end]
                     (= (get-y splice-end) midpoint-height)
                     [splice-start
                      [(get-x splice-start) (get-y splice-end)]
                      splice-end]))]
    (splice-list path splicing start-index (count crossing))))

(defn groovify
  [path bottom?]
  (let [left-limit (->> path
                     (map get-x)
                     (apply min)
                     next-groove-start)
        right-limit (->> path
                      (map get-x)
                      (apply max)
                      prev-groove-end)]
    (reduce (fn [path [x license-to-kill?]]
              (let [left (prev-groove-start x)
                    right (next-groove-end x)]
                (reduce (fn [path midpoint-height-result]
                          (splice path
                                  midpoint-height-result
                                  left right))
                        path
                        (midpoint-heights path ; doesn't matter if this is original or modified path
                                          (crossings path left right)
                                          left right
                                          bottom?
                                          (and (not bottom?) license-to-kill?)))))
            path
            (let [xs (range (+ left-limit half-thickness)
                            right-limit
                            twice-thickness)]
              (case (count xs)
                0 nil
                1 [[(first xs) true]]
                (concat [[(first xs) true]]
                        (map #(vector % false)
                             (butlast (next xs)))
                        [[(last xs) true]]))))))

(defn inchify
  [path]
  (let [left (apply min (map get-x path))
        top (apply max (map get-y path))]
    (vec
      (map (fn [[x y]]
             [(* (- x left)
                 (/ material-thickness thickness))
              (* (- top y)
                 (/ material-thickness thickness))])
           path))))
