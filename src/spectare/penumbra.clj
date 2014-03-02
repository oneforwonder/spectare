(ns spectare.penumbra
  (:use [penumbra.opengl]
        [spectare.util])
  (:require [penumbra.app :as app]))

; Triangles
(defn make-tri [] 
  {:shape  :tri
   :col   (rand-color) 
   :center [(rand-int 800) (rand-int 600)]
   :size   30  ; height
   :angle  0})

;(def ts (repeatedly 20 (make-tri)))

(defn tri-points [height]
  [0
   (- (* 2 (/ height 3)))
   (- (/ height 1.732))
   (+ (/ height 3))
   (+ (/ height 1.732))
   (+ (/ height 3))])

(defn center-tri [[cx cy] [x1 y1 x2 y2 x3 y3]] 
  [(+ x1 cx)
   (+ y1 cy)
   (+ x2 cx)
   (+ y2 cy)
   (+ x3 cx)
   (+ y3 cy)])

; Penumbra
(defn init [state]
    (app/vsync! true)
    state)

(defn reshape [[x y width height] state]
  (println "reshape")
  (frustum-view 60.0 (/ (double width) height) 1.0 100.0)
  (load-identity)
  state)

(defn display [[delta time] state]
  (println "display")
  ;(translate 0 -0.93 -3)
  ;(rotate (rem (* 90 time) 360) 0 1 0)
  (doseq [t (repeatedly 20 make-tri)]
    (println "past rep")
    (let [{:keys [size col center]} t
          [x1 y1 x2 y2 x3 y3] (center-tri center (tri-points size))]
      (draw-triangles
        (apply color col) 
        (vertex x1 y1)
        (vertex x2 y2)
        (vertex x3 y3))))
  (app/repaint!))

(defn -main []
(app/start
    {:display display, :reshape reshape, :init init}
    {}))
