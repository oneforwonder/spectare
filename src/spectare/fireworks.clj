(ns spectare.fireworks
  (:require [spectare.geometry :as geo])
  (:use spectare.util
        quil.core)) 

(def WIDTH (min (screen-width) 1920))
(def HEIGHT (screen-height))

(def FPS 10)

(def SPIN? false)
(def LENGTH 20.0)
(def LINE-THICKNESS 2)
(def RING-COUNT 7)

(def frame-num (atom 0))

;; Lines
(defn rand-pos []
  [(rand-int WIDTH) (rand-int HEIGHT)])

(defn make-line [color [cx cy] degrees ring-num]
  (let [[dx dy] (geo/rotate-point degrees [LENGTH 0])]
    {:x1 (+ cx (* dx (* 1.04 ring-num)))
     :y1 (+ cy (* dy (* 1.04 ring-num)))
     :x2 (+ cx (* dx (inc ring-num)))
     :y2 (+ cy (* dy (inc ring-num)))
     :col color}))

(defn make-burst-data [n]
  (let [center   (rand-pos)
        base-col (rand-vivid-hsb-color)
        colors   (iterate shift-hsb-hue base-col)]
    (for [ring-num (range RING-COUNT)]
      (for [degrees (range 0 360 (- 5 (* 0.5 ring-num)))]
        {:col (nth colors ring-num)
         :center center
         :degrees degrees
         :ring-num ring-num}))))

(defn make-line-from-data [data]
  (let [{:keys [col center degrees ring-num]} data]
    (make-line col center degrees ring-num)))

;; Quil fns
(defn setup []
  (def rings (mapcat make-burst-data (range)))
  (def burst-data (make-burst-data 0))
  (color-mode :hsb 256)
  (background 0 0 0)
  (smooth)
  (frame-rate FPS))

(defn draw-line! [{:keys [x1 y1 x2 y2 col]}] 
  (stroke-weight LINE-THICKNESS)
  (apply stroke col)
  (line x1 y1 x2 y2))

(defn draw-bg! [alpha]
  (no-stroke)
  (fill 0 0 0 alpha)
  (rect 0 0 WIDTH HEIGHT))

(defn update-flashing []
  (draw-bg! 80)
  (doseq [l (nth rings @frame-num)]
    (draw-line! (make-line-from-data l)))
  (swap! frame-num inc))

(defn update []
  (draw-bg! 255)
  (doseq [ring-data burst-data]
    (let [rotate-factor (case (mod (:ring-num (first ring-data)) 2) 0 -1 1 1)
          rotated-lines (map (fn [line-data] 
                               (let [d (:degrees line-data)] 
                                 (assoc line-data :degrees (+ d (* @frame-num rotate-factor 0.5))))) ring-data)
          colored-lines (map (fn [line-data change?]
                               (let [c (:col line-data)
                                     b (last c)]
                                 (assoc line-data :col (if change? (assoc c 2 (- b 80)) c)))) 
                             rotated-lines (cycle [true false]))]
      (doseq [l (map make-line-from-data colored-lines)]
        (draw-line! l))))
  (swap! frame-num inc))

(defn -main []
  (defsketch fireworks
    :title "fireworks"
    :setup setup
    :draw (if SPIN? update update-flashing)
    :size [WIDTH HEIGHT]))
