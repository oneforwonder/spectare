(ns spectare.sine
  (:use spectare.util
        quil.core)) 

(def WIDTH (screen-width))
(def HEIGHT (screen-height))

(def CLEAR-SCREEN false)
(def SCALE 20)
(def FPS 20)

(def frame-num (atom 0))

(defn s [x & ys]
  (apply * SCALE x ys))

;; Quil fns
(defn setup []
  (smooth)
  (frame-rate FPS))

(defn draw! [x dy]
  (let [col [(+ x 80) 
             (- 130 x) 
             (+ x (/ dy 3) 160)]]
    (no-stroke)
    (apply fill col)
    (ellipse (- (s x) SCALE) (+ (/ HEIGHT 2) dy (s (Math/sin x))) SCALE SCALE))) 

(defn update []
  (when CLEAR-SCREEN (background 220 220 220))
  (let [x (float (/ @frame-num 3))]
    (draw! x 0)
    (doseq [dy (range 11)]
      (draw! x (s (- dy 5) 1.4))))
  (swap! frame-num inc))

(defn -main []
  (defsketch sine
    :title "sine"
    :setup setup
    :draw update
    :size [WIDTH HEIGHT]))
