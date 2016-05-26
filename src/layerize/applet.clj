; Copyright © 2015-2016 Radon Rosborough. All rights reserved.
(ns layerize.applet
  (:require [layerize.equation :as eq]
            [layerize.trace :refer :all]
            [layerize.util :as util :refer [get-x get-y get-ul get-lr]]
            [quil.core :as q]
            [quil.middleware :as qm]))

;;; UI config

(def point-size
  0.0015)

(def path-size
  10)

(def window-size
  850)

(def movement-fraction
  0.1)

(def zoom-factor
  1.2)

(def offset-multiple
  1.2)

;;; Helper functions

(defn scale-radius
  [radius state]
  (* (/ radius
        (- (-> state :bounds get-lr get-x)
           (-> state :bounds get-ul get-x))
        (- (-> state :viewing-frame get-lr get-x)
           (-> state :viewing-frame get-ul get-x)))
     (#(* % %) (- (-> state :window-frame get-lr get-x)
                  (-> state :window-frame get-ul get-x)))))

(defn reset-points
  [offset]
  (print "Recalculating points... ") (flush)
  (let [res (vec (eq/cross-section (eq/point-mesh 2000 2000)
                                   1 offset 0.03))]
    (println "done.")
    res))

(defn reset-queue
  [state]
  (assoc state
         :queue (concat ; Current position
                        (if (:cur-pos state)
                          (map vector
                               (repeat (:cur-pos state))
                               [[0 255 0] [0 0 255]]
                               [(scale-radius outer-tolerance state)
                                (scale-radius inner-tolerance state)]))
                        ; Points
                        (map vector
                             (:points state)
                             (repeat [0 0 0])
                             (repeat (scale-radius point-size state)))
                        ; Current path
                        (if (#{:begin-tracing :tracing} (:stage state))
                          (map vector
                               (:path state)
                               (repeat [255 0 0])
                               (repeat path-size)))
                        ; Completed paths
                        (map vector
                             (apply concat (:paths state))
                             (repeat [255 0 0])
                             (repeat path-size)))
         :clear :pre
         :done 0))

;;; Quil functions

(declare update)

(defn setup
  []
  (q/frame-rate 60)
  (let [points (reset-points 0)
        bounds (util/bounds points)
        window-frame (util/squarify bounds
                                    [[0 0]
                                     [window-size window-size]]
                                    [path-size path-size])
        reversed-window-frame [[(-> window-frame get-ul get-x)
                                (- window-size
                                   (-> window-frame get-ul get-y))]
                               [(-> window-frame get-lr get-x)
                                (- window-size
                                   (-> window-frame get-lr get-y))]]]
    (update
      {; Algorithm
       :points points

       ; Viewing
       :bounds bounds
       :window-frame window-frame
       :reversed-window-frame reversed-window-frame
       :viewing-frame window-frame
       :offset 0
       :offset-inc 0.5
       :offset-buffer ""

       ; Rendering
       :queue (map vector
                   points
                   (repeat [0 0 0])
                   (repeat (scale-radius point-size {:bounds bounds
                                                     :window-frame window-frame
                                                     :viewing-frame window-frame})))
       :clear :pre
       :done 0
       })))

(defn key-typed
  [state {key-word :key}]
  (let [size (max (- (-> state :viewing-frame get-lr get-x)
                     (-> state :viewing-frame get-ul get-x))
                  (- (-> state :viewing-frame get-lr get-y)
                     (-> state :viewing-frame get-ul get-y)))]
    (case key-word
      :a (-> state
           (update-in [:viewing-frame 0 0] - (* size movement-fraction))
           (update-in [:viewing-frame 1 0] - (* size movement-fraction))
           reset-queue)
      :d (-> state
           (update-in [:viewing-frame 0 0] + (* size movement-fraction))
           (update-in [:viewing-frame 1 0] + (* size movement-fraction))
           reset-queue)
      :w (-> state
           (update-in [:viewing-frame 0 1] - (* size movement-fraction))
           (update-in [:viewing-frame 1 1] - (* size movement-fraction))
           reset-queue)
      :s (-> state
           (update-in [:viewing-frame 0 1] + (* size movement-fraction))
           (update-in [:viewing-frame 1 1] + (* size movement-fraction))
           reset-queue)
      := (let [width (- (-> state :viewing-frame get-lr get-x)
                        (-> state :viewing-frame get-ul get-x))
               height (- (-> state :viewing-frame get-lr get-y)
                         (-> state :viewing-frame get-ul get-y))]
           (-> state
             (update-in [:viewing-frame 0 0] + (-> zoom-factor / - inc (* width 1/2)))
             (update-in [:viewing-frame 1 0] - (-> zoom-factor / - inc (* width 1/2)))
             (update-in [:viewing-frame 0 1] + (-> zoom-factor / - inc (* height 1/2)))
             (update-in [:viewing-frame 1 1] - (-> zoom-factor / - inc (* height 1/2)))
             reset-queue))
      :-( let [width (- (-> state :viewing-frame get-lr get-x)
                        (-> state :viewing-frame get-ul get-x))
               height (- (-> state :viewing-frame get-lr get-y)
                         (-> state :viewing-frame get-ul get-y))]
          (-> state
            (update-in [:viewing-frame 0 0] - (-> zoom-factor dec (* width 1/2)))
            (update-in [:viewing-frame 1 0] + (-> zoom-factor dec (* width 1/2)))
            (update-in [:viewing-frame 0 1] - (-> zoom-factor dec (* height 1/2)))
            (update-in [:viewing-frame 1 1] + (-> zoom-factor dec (* height 1/2)))
            reset-queue))
      :o (let [offset-inc (* (:offset-inc state) offset-multiple)]
           (println (str "Offset increment is now " offset-inc "."))
           (assoc state :offset-inc offset-inc))
      :k (let [offset-inc (/ (:offset-inc state) offset-multiple)]
           (println (str "Offset increment is now " offset-inc "."))
           (assoc state :offset-inc offset-inc))
      :p (let [offset (+ (:offset state) (:offset-inc state))]
           (println (str "Offset is now " offset "."))
           (-> state
             (assoc
               :offset offset
               :points (reset-points offset)
               :paths []
               :path []
               :stage nil)
             reset-queue))
      :l (let [offset (- (:offset state) (:offset-inc state))]
           (println (str "Offset is now " offset "."))
           (-> state
             (assoc
               :offset offset
               :points (reset-points offset)
               :paths []
               :path []
               :stage nil)
             reset-queue))
      :j (do
           (print "Offset is now ")
           (flush)
           (assoc state :offset-buffer ""))
      (:0 :1 :2 :3 :4 :5 :6 :7 :8 :9 :. :n)
      (let [key-word (if (#{:n} key-word) :- key-word)]
        (print (name key-word))
        (flush)
        (update-in state [:offset-buffer] str (name key-word)))
      :i (let [offset (Double/parseDouble (:offset-buffer state))]
           (println ".")
           (-> state
             (assoc
               :offset offset
               :points (reset-points offset)
               :paths []
               :path []
               :stage nil)
             reset-queue))
      :g (do
           (print "g") (flush)
           (let [new-state (merge state (step-trace state))]
             (print "√") (flush)
             (reset-queue new-state)))
      :G (do
           (print "G") (flush)
           (let [new-state (nth (iterate #(merge % (step-trace %)) state) 10)]
             (print "√") (flush)
             (reset-queue new-state)))
      :f (do
           (print "f") (flush)
           (let [new-state (nth (iterate #(merge % (step-trace %)) state) 50)]
             (print "√") (flush)
             (reset-queue new-state)))
      :F (do
           (print "F") (flush)
           (let [new-state (nth (iterate #(merge % (step-trace %)) state) 500)]
             (print "√") (flush)
             (reset-queue new-state)))
:D (do
     (print "Tracing... ") (flush)
     (let [new-state (util/first-where (comp #{:done} :stage)
                                       (iterate #(merge % (step-trace %)) state))]
       (println "done.")
       (reset-queue new-state)))
:T (loop [offset (- (rand 19) 10)]
     (println (str "Selected offset " offset "."))
     (let [points (reset-points offset)
           _ (do (print "Attempting trace... ") (flush))
           new-state (nth (iterate #(merge % (try (step-trace %) (catch clojure.lang.ArityException _ {:failed true})))
                                   (assoc state
                                          :offset offset
                                          :points points
                                          :paths []
                                          :path []
                                          :stage nil)) 5000)]
       (if (#{:done} (:stage new-state))
         (do
           (println "success!")
           (recur (- (rand 19) 10)))
         (do
           (println (if (:failed new-state) "failed (with ArityException)." "failed."))
           (reset-queue (dissoc new-state :failed))))))
state)))

(defn update
  [state]
  (let [new-queue (drop (:done state) (:queue state))]
    (assoc state
           :queue new-queue
           :done (count new-queue)
           :clear (if (= (:clear state)
                         :pre)
                    :post))))

(defn draw
  [state]
  (if (:clear state)
    (q/background 240))
  (dorun (for [[[x y] color radius]
               (map vector
                    (util/rescale
                      (util/rescale
                        (map first (:queue state))
                        (:bounds state)
                        (:reversed-window-frame state))
                      (:viewing-frame state)
                      (:window-frame state))
                    (map second (:queue state))
                    (map #(nth % 2) (:queue state)))]
           (do
             (apply q/fill color)
             (apply q/stroke color)
             (q/ellipse x y radius radius))))
  (assoc state
         :queue (empty (:queue state))
         :clear false))

(q/defsketch
  trace-applet
  :title "Path Tracer"
  :size [window-size window-size]
  :setup setup
  :update update
  :draw draw
  :key-typed key-typed
  :features [:keep-on-top]
  :middleware [qm/fun-mode])
