(ns layerize.svg
  (:require [clojure.string :as string]
            [layerize.schematic :as schematic]
            [layerize.util :as util :refer [get-x get-y]]))

; An SVG tag is represented as a vector whose first
; element is the tag name, whose second element is
; a map of attributes to values for the tag, and whose
; remaining elements are any tags

;;; Convert SVG data structures to strings

(defn svg->str
  [tag]
  (if (string? tag)
    tag
    (let [[tag-name attrs & subtags] tag]
      (str "<"
           tag-name
           (if (seq attrs) " ")
           (string/join " "
                        (map (fn [[attr-name attr-value]]
                               (str attr-name
                                    "=\""
                                    attr-value
                                    "\""))
                             attrs))
           (if (seq subtags)
             (if (and (= (count subtags) 1)
                      (string? (first subtags)))
               (str ">" (first subtags) "</" tag-name ">")
               (str ">\n"
                    (string/join "\n"
                                 (for [line (mapcat #(string/split (svg->str %) #"\n")
                                                    subtags)]
                                   (str "\t" line)))
                    "\n</"
                    tag-name
                    ">"))
             " />")))))

(defn point->str
  [[x y]]
  (str x "," y ""))

;;; Generate SVG data structures

(defn document
  [& subtags]
  (reduce conj
          ["svg" {"version" "1.1"
                  "xmlns" "http://www.w3.org/2000/svg"
                  "viewBox" "0 0 10 8"
                  "width" "10in"
                  "height" "8in"}]
          subtags))

(defn group
  [attrs & subtags]
  (reduce conj
          ["g" attrs]
          subtags))

(defn text
  [[x y] content font-size]
  ["text"
   {"x" x
    "y" y
    "fill" "black"
    "font-size" font-size
    "font-family" "tinos, serif"}
   content])

(defn polygon
  [points stroke-width stroke]
  ["polygon" {"points" (string/join " "
                                    (map point->str
                                         points))
              "fill" "none"
              "stroke" stroke
              "stroke-width" stroke-width}])

;;; Process piece schematics

(defn pieces->svg
  "offsets: [[x-left x-right] [y-left y-right]]"
  ([pieces offsets conflicts]
   (svg->str
     (apply document
            (for [dimension (range (count pieces))
                  offset (range (count (get pieces dimension)))
                  lr (range (count (get-in pieces [dimension offset])))
                  :let [piece (get-in pieces [dimension offset lr])
                        piece (if offsets
                                piece (schematic/inchify piece))
                        [x y] [(* dimension 20) (* offset 7)]]]
              (apply group
                     (let [flip
                           (fn [[x y]]
                             [x (- y)])]
                       (list*
                         {"transform"
                          (str "translate(" x "," y")")}
                         (polygon (map flip piece)
                                  (if offsets "0.005" "0.001")
                                  "black")
                         (text
                           (flip [(apply util/average (map get-x piece))
                                  (apply util/average (map get-y piece))])
                           (str
                             (nth "XY" dimension)
                             offset
                             (nth "LR" lr))
                           (if offsets "0.3" "0.1"))
                         (when offsets
                           (let [[[x-left x-right] [y-left y-right]] offsets]
                             (concat (map-indexed (fn [index transverse-offset]
                                                    (text
                                                      (flip [transverse-offset 0])
                                                      (str index)
                                                      (if offsets "0.15" "0.05")))
                                                  (range (nth [x-left y-left] dimension)
                                                         (nth [x-right y-right] dimension)
                                                         schematic/twice-thickness))
                                     (->> conflicts
                                       (filter (fn [conflict]
                                                 (and (= dimension (:dimension conflict))
                                                      (= offset (:offset conflict))
                                                      (= lr (:lr conflict)))))
                                       (map (fn [{:keys [left right bottom top]}]
                                              (polygon (map flip [[left bottom]
                                                                  [right bottom]
                                                                  [right top]
                                                                  [left top]])
                                                       "0.01"
                                                       "red"))))))))))))))
  ([pieces] (pieces->svg pieces nil nil)))
