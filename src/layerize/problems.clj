; Copyright © 2015-2016 Radon Rosborough. All rights reserved.
(ns layerize.problems
  (:require clojure.set
            [clojure.string :as string]
            [layerize.schematic :as schematic]
            [layerize.util :as util :refer [get-x get-y]]))

(defn min-max
  [crossing]
  {:min (apply min (map get-y crossing))
   :max (apply max (map get-y crossing))})

(defn crossings
  [path left right]
  (let [left (+ left 0.00000000001)
        right (- right 0.00000000001)
        within? (partial util/strictly-between? left right)
        first-outside-index (inc (count (take-while within?
                                                    (map get-x path))))
        shifted-path (util/rotate path first-outside-index)
        initial-state (if (< (-> shifted-path first get-x)
                             left)
                        :on-left :on-right)]
    (loop [path shifted-path
           state initial-state
           crossings []
           cur-crossing nil]
      (if (seq path)
        (case state
          :on-left (if (> (-> path first get-x)
                          left)
                     (recur path
                            :from-left
                            crossings
                            [(first path)])
                     (recur (rest path)
                            :on-left
                            crossings
                            nil))
          :on-right (if (< (-> path first get-x)
                           right)
                      (recur path
                             :from-right
                             crossings
                             [(first path)])
                      (recur (rest path)
                             :on-right
                             crossings
                             nil))
          :from-left (cond
                       (>= (-> path first get-x)
                           right)
                       (recur (rest path)
                              :on-right
                              (conj crossings
                                    (assoc (min-max cur-crossing)
                                           :type :total))
                              nil)
                       (<= (-> path first get-x)
                           left)
                       (recur (rest path)
                              :on-left
                              (conj crossings
                                    (assoc (min-max cur-crossing)
                                           :type :partial))
                              nil)
                       :otherwise
                       (recur (rest path)
                              :from-left
                              crossings
                              (conj cur-crossing (first path))))
          :from-right (cond
                        (<= (-> path first get-x)
                            left)
                        (recur (rest path)
                               :on-left
                               (conj crossings
                                     (assoc (min-max cur-crossing)
                                            :type :total))
                               nil)
                        (>= (-> path first get-x)
                            right)
                        (recur (rest path)
                               :on-right
                               (conj crossings
                                     (assoc (min-max cur-crossing)
                                            :type :partial))
                               nil)
                        :otherwise
                        (recur (rest path)
                               :from-right
                               crossings
                               (conj cur-crossing (first path)))))
        crossings))))

(defn boundaries
  [crossings]
  (loop [crossings (sort-by :min
                            crossings)
         bounds []
         state :outside]
    (if-let [crossing (first crossings)]
      (case state
        :outside
        (case (:type crossing)
          :total
          (recur (rest crossings)
                 (conj bounds (:min crossing))
                 :inside)
          :partial
          (recur (rest crossings)
                 (reduce conj bounds [(:min crossing)
                                      (:max crossing)])
                 :outside))
        :inside
        (case (:type crossing)
          :total
          (recur (rest crossings)
                 (conj bounds (:max crossing))
                 :outside)
          :partial
          (recur (rest crossings)
                 bounds
                 :inside)))
      bounds)))

(defn sample
  [bounds bottom top spacing]
  (loop [bounds bounds
         y bottom
         inside? false
         samples []]
    (if (< y top)
      (let [passed (count (take-while #(< % y)
                                      bounds))
            inside? (if (odd? passed)
                      (not inside?)
                      inside?)]
        (recur (drop passed bounds)
               (+ y spacing)
               inside?
               (conj samples [y inside?])))
      (->> samples
        (filter second)
        (map first)))))

(defn intersect
  [spacing samples1 samples2]
  (sort (clojure.set/intersection
          (set samples1)
          (set samples2))))

(defn consolidate
  [spacing points]
  (map (fn [pairs]
         (let [ys (util/depair pairs)]
           {:y (apply util/average ys)
            :size (count ys)}))
       (util/drop-every-nth
         (partition-by
           (fn [[a b]]
             (< (- b a)
                (* spacing 1.5)))
           (partition 2 1 points))
         2)))

(def lower-bound -4)
(def upper-bound 4)
(def spacing 0.01)

(defn conflicts
  "piece: [path left right]"
  [piece1 piece2]
  (->> (map (fn [[path left right]]
             (-> path
               (crossings left right)
               boundaries
               (sample lower-bound upper-bound spacing)))
           [piece1 piece2])
    (apply intersect spacing)
    (consolidate spacing)))

(defn all-conflicts
  [pieces left-bound right-bound]
  (let [offsets (vec (range left-bound right-bound schematic/twice-thickness))
        dimension 0
        transverse-dimension 1]
    (apply concat
           (for [offset (range (count (get pieces dimension)))
                 lr (range (count (get-in pieces [dimension offset])))
                 transverse-offset (range (count (get pieces transverse-dimension)))
                 transverse-lr (range (count (get-in pieces [transverse-dimension transverse-offset])))
                 :let [piece (get-in pieces [dimension offset lr])
                       transverse-piece (get-in pieces [transverse-dimension transverse-offset transverse-lr])
                       transverse-left (- (nth offsets offset) schematic/half-thickness)
                       transverse-right (+ (nth offsets offset) schematic/half-thickness)
                       left (- (nth offsets transverse-offset) schematic/half-thickness)
                       right (+ (nth offsets transverse-offset) schematic/half-thickness)
                       conflicts (conflicts [piece left right]
                                            [transverse-piece transverse-left transverse-right])]
                 conflict conflicts]
             [{:dimension dimension
               :offset offset
               :lr lr
               :left left
               :right right
               :bottom (- (:y conflict)
                          (* (:size conflict) spacing 1/2))
               :top (+ (:y conflict)
                       (* (:size conflict) spacing 1/2))}
              {:dimension transverse-dimension
               :offset transverse-offset
               :lr transverse-lr
               :left transverse-left
               :right transverse-right
               :bottom (- (:y conflict)
                          (* (:size conflict) spacing 1/2))
               :top (+ (:y conflict)
                       (* (:size conflict) spacing 1/2))}]))))

(defn print-conflicts
  [conflicts]
  (println "Piece Piece Height Size")
  (println "----- ----- ------ ----")
  (->> conflicts
    (partition 2)
    (map (fn [[xc yc]]
           {:xpiece (str "X"
                         (:offset xc)
                         (nth "LR" (:lr xc)))
            :ypiece (str "Y"
                         (:offset yc)
                         (nth "LR" (:lr yc)))
            :height (format "%f" (util/average (:top xc) (:bottom xc)))
            :size (format "%f" (- (:top xc) (:bottom xc)))}))
    (sort-by :size)
    (map (fn [{:keys [xpiece ypiece height size]}]
           (format "%5s %5s %6s %4s"
                   xpiece ypiece height size)))
    (string/join "\n")
    println))
