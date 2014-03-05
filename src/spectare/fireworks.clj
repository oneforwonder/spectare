(ns spectare.fireworks
  (:require [spectare.geometry :as geo])
  (:use spectare.util
        quil.core)) 

(def WIDTH (min (screen-width) 1920))
(def HEIGHT (screen-height))

(def LENGTH 20)
(def LINE-THICKNESS 2)
(def RING-COUNT 7)

(def FPS 10)

(def frame-num (atom 0))

;; Lines
(defn rand-pos []
  [(rand-int WIDTH) (rand-int HEIGHT)])

(defn make-line [color [cx cy] degrees ring-num]
  (let [[dx dy] (geo/rotate-point degrees [LENGTH 0])]
    {:x1 (+ cx (* dx (* 1.1 ring-num)))
     :y1 (+ cy (* dy (* 1.1 ring-num)))
     :x2 (+ cx (* dx (inc ring-num)))
     :y2 (+ cy (* dy (inc ring-num)))
     :col color}))

(defn make-ring [n]
  (let [center   (rand-pos)
        base-col (rand-vivid-hsb-color)
        colors   (iterate shift-hsb-hue base-col)]
    (for [ring-num (range RING-COUNT)]
      (for [degrees (range 0 360 (- 5 (* 0.5 ring-num)))]
        (make-line (nth colors ring-num) center degrees ring-num))))) 

;; Quil fns
(defn setup []
  (def rings (mapcat make-ring (range)))
  (color-mode :hsb 256)
  (background 0 0 0)
  (smooth)
  (frame-rate FPS))

(defn draw-line! [{:keys [x1 y1 x2 y2 col]}] 
  (stroke-weight LINE-THICKNESS)
  (apply stroke col)
  (line x1 y1 x2 y2))

(defn draw-bg! []
  (no-stroke)
  (fill 0 0 0 80)
  (rect 0 0 WIDTH HEIGHT))

(defn update []
  (draw-bg!)
  (doseq [l (nth rings @frame-num)]
    (draw-line! l))
  (swap! frame-num inc))

(defn -main []
  (defsketch fireworks
    :title "fireworks"
    :setup setup
    :draw update
    :size [WIDTH HEIGHT]))
