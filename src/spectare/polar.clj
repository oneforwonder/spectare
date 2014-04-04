(ns spectare.polar
  (:use spectare.util
        quil.core)) 

(def WIDTH (min (screen-width) 1920))
(def HEIGHT (screen-height))
(def CENTER [(/ WIDTH 2) (/ HEIGHT 2)])

(def FPS 180)

(def CIRCLE-RADIUS (* 0.40 HEIGHT))
(def POINT-SIZE 3)
(def SAMPLE-RATE 172)

(def frame-num (atom 0))

;; Helpers
(defn polar-to-cartesian [angle radius]
  [(* (Math/cos angle) radius) 
   (* (Math/sin angle) radius)])

;; Waves
(def pi Math/PI)
(def half-pi (/ pi 2))
(def two-pi (* pi 2))

(defn sin-wave [samples-per-cycle]
  (cycle (map #(/ (+ 1.0 (Math/sin %)) 2.0) (range (* 3 half-pi) (* 7 half-pi) (/ two-pi samples-per-cycle)))))

(defn tri-wave [samples-per-cycle]
  (let [half (/ samples-per-cycle 2)]
    (cycle (concat (range 0.0 1.0 (/ 1.0 half))
                   [1]
                   (range 1.0 0.0 (- (/ 1.0 half)))
                   [0]))))

(defn step-wave []
  (cycle
  (mapcat #(repeat 4 %) (range 0.0 1.0 0.1))))

;; Quil fns
(defn setup []
  (def angles (iterate #(+ 0.01 %) 0.0))
  ;(def dists  (sin-wave SAMPLE-RATE))
  (def dists  (tri-wave SAMPLE-RATE))
  ;(def dists (step-wave))
  (background 0 0 0)
  (smooth)
  (frame-rate FPS))

(defn draw-point [angle dist]
  (let [[x y] (polar-to-cartesian angle dist)]
    (println "Dist:" dist)
    (println [x y])
    (no-stroke)
    (fill 255 255 255)
    (ellipse (+ x (/ WIDTH 2)) (+ y (/ HEIGHT 2)) POINT-SIZE POINT-SIZE)))

(defn draw-bg! []
  (no-stroke)
  (fill 0 0 0)
  (rect 0 0 WIDTH HEIGHT))

(defn update []
  (let [a (nth angles @frame-num)
        d (nth dists @frame-num)]
    (println "fn:" @frame-num)
    (println "A:" a)
    (println "D:" d)
    (draw-point a (* d CIRCLE-RADIUS)))
  (println)
  (swap! frame-num inc))

(defn -main []
  (defsketch polar
    :title "polar"
    :setup setup
    :draw  update
    :size [WIDTH HEIGHT]))

