(ns spectare.grid
  (:require [spectare.geometry :as geo])
  (:use spectare.util
        quil.core)) 

(def WIDTH (min (screen-width) 1920))
(def HEIGHT (screen-height))

(def FPS 30)

(def MOUSE-RANGE 300)
(def SPACING 20)
(def SIZE 2)

(defn point-size [p]
  (let [dx   (abs (- (:x p) (mouse-x)))
        dy   (abs (- (:y p) (mouse-y)))
        dist (Math/sqrt (+ (* dx dx) (* dy dy)))
        inv  (- MOUSE-RANGE (min dist MOUSE-RANGE))]
    (inc (/ inv 30))))

(defn make-grid []
  (for [x (range 0 WIDTH SPACING)
        y (range 0 HEIGHT SPACING)]
    {:x x :y y}))

(defn setup []
  (color-mode :hsb 256)
  (background 0 0 0)
  (smooth)
  (frame-rate FPS))

(defn draw-point! [p] 
  (let [{:keys [x y]} p
        size (point-size p)]
    (no-stroke)
    (apply fill [0 0 255])
    (ellipse x y size size)))

(defn draw-bg! []
  (no-stroke)
  (fill 0 0 0)
  (rect 0 0 WIDTH HEIGHT))

(defn update []
  (draw-bg!)
  (doseq [p (make-grid)]
    (draw-point! p)))

(defn -main []
  (defsketch grid
    :title "grid"
    :setup setup
    :draw update
    :size [WIDTH HEIGHT]))

