(ns spectare.parallel
  (:use spectare.util
        quil.core)) 

(def WIDTH (screen-width))
(def HEIGHT (screen-height))

(def FPS 1)

(def LINE-THICKNESS 2)

(defn rand-pos []
  [(rand-int WIDTH) (rand-int HEIGHT)])

;; Lines
(defn make-line [c1 c2 [x1 y1] [x2 y2] dx dy i n]
  {:x1 (+ x1 (* dx i LINE-THICKNESS))
   :y1 (+ y1 (* dy i LINE-THICKNESS))
   :x2 (+ x2 (* dx i LINE-THICKNESS))
   :y2 (+ y2 (* dy i LINE-THICKNESS))
   :col (gradient c1 c2 (/ i n))})

(defn make-lines []
  (let [c1 (rand-color)
        c2 (rand-color)
        p1 (rand-pos)
        p2 (rand-pos)
        dx (+- (rand-int 5))
        dy (+- (rand-int 5))
        n  (+ 60 (rand-int 100))]
    (map #(make-line c1 c2 p1 p2 dx dy % n) (range n))))

;; Quil fns
(defn setup []
  (smooth)
  (frame-rate FPS))

(defn draw! [lines]
  (stroke-weight LINE-THICKNESS)
  (doseq [{:keys [x1 y1 x2 y2 col]} lines]
    (apply stroke col)
    (line x1 y1 x2 y2)))

(defn update []
  (draw! (make-lines)))

(defn -main []
  (defsketch parallel
    :title "parallel"
    :setup setup
    :draw update
    :size [WIDTH HEIGHT]))
