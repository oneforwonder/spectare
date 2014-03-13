(ns spectare.drawing
  (:use spectare.util
        quil.core)) 

(def WIDTH  512)
(def HEIGHT 512)

(def TRI-COUNT 50)

(def FPS 1)

(defn rand-pos []
  [(rand-int WIDTH) (rand-int HEIGHT)])

(defn make-triangle []
  {:p1  (rand-pos)
   :p2  (rand-pos)
   :p3  (rand-pos)
   :col (rand-rgba-color)})

;; Quil fns
(defn setup []
  (def triangles (take TRI-COUNT (repeatedly make-triangle)))
  (no-stroke)
  (background 0 0 0)
  (smooth)
  (frame-rate FPS))

(defn draw-triangle! [{:keys [p1 p2 p3 col]}] 
  (let [[[x1 y1] [x2 y2] [x3 y3]] [p1 p2 p3]]
    (apply fill col) 
    (triangle x1 y1 x2 y2 x3 y3)))

(defn update []
  (doseq [tri triangles]
    (draw-triangle! tri))) 

(defn -main []
  (defsketch drawing
    :title "drawing"
    :setup setup
    :draw update 
    :size [WIDTH HEIGHT]))
