(ns spectare.grow
  (:require clisk.patterns)
  (:use spectare.util
        quil.core)
  (:gen-class)) 

(def WIDTH (min (screen-width) 1920))
(def HEIGHT (screen-height))

(def FPS 60)

(def START-SHAPE :tri)
(def ALT-SHAPE :circle)
(def CHANGE-SHAPES? true)
(def CHANGE-EVERY 160)
(def START-SIZE 1)
(def ROTATE? true)
(def ROTATION-FACTOR 1)
(def GROWTH-STYLE :mixed)    ; Either :add, :mult, or :mixed
(def GROWTH-ADD-AMOUNT 2)    ; Used with :add
(def GROWTH-MULT-RATIO 1.03) ; Used with :mult
(def FRAMES-BETWEEN-NEW 5)
(def STROKE? true)
(def OPACITY 255)

(def frame-num (atom 0))
(def to-draw (atom []))

(def current-center (atom [(/ WIDTH 2) (/ HEIGHT 2)]))
(def current-velocity (atom [0 0]))
(def next-direction-frame (atom 0))

(defn move-center [center]
  (map + center @current-velocity))

;; Shapes
(defn rand-hsb-color []
  [(rand-int 256) 255 180])

(defn make-circle 
  ([] (make-circle (rand-color)))
  ([color] {:shape  :circle
            :center @current-center
            :color  [(mod (+ (first color) 5 (+- (brand-int 10 30))) 256) 255 180] 
            :size   START-SIZE})) ; diameter

(defn angle []
  (let [num @frame-num
        alt (= 1 (mod (quot num 120) 2))
        base (if alt (- 121(mod num 120)) num)]
    (if ROTATE?
      (* ROTATION-FACTOR num)
      0)))

(defn make-tri
  ([] (make-tri (rand-color)))
  ([color] {:shape  :tri
            :center @current-center
            :color  [(mod (+ (first color) 5 (+- (brand-int 10 30))) 256) 255 180]
            :size   START-SIZE  ; height
            :angle  (angle)}))

(defn grow-shape [s]
  (let [size (:size s)]
    (assoc s :size (case GROWTH-STYLE
                     :add   (+ size GROWTH-ADD-AMOUNT)
                     :mult  (* size GROWTH-MULT-RATIO)
                     :mixed (max (+ size GROWTH-ADD-AMOUNT) (* size GROWTH-MULT-RATIO)))))) 

(defn grow-shapes [ss]
  (map grow-shape ss))

(defn remove-large-shapes [ss]
  (remove (fn [s]
            (case (:shape s)
              :circle (> (:size s) (* 1.4 WIDTH))
              :tri    (> (:size s) (* 2.4 WIDTH)))) ss))

(defn new-shape? []
  (and (or (not CHANGE-SHAPES?) (< 12 (mod @frame-num CHANGE-EVERY)))
       (= 0 (mod @frame-num FRAMES-BETWEEN-NEW))))

(defn new-shape []
  (let [prev-color (or (:color (last @to-draw)) (rand-color))
        shape      (if (= 1 (mod (quot @frame-num CHANGE-EVERY) 2)) ALT-SHAPE START-SHAPE)
        shape-fn   ({:circle make-circle :tri make-tri} shape)]
    (shape-fn prev-color)))

;; Geometry
(defn tri-points [height]
  [0
   (- (* 2 (/ height 3)))
   (- (/ height 1.732))
   (+ (/ height 3))
   (+ (/ height 1.732))
   (+ (/ height 3))])

(defn rotate-tri [angle [x1 y1 x2 y2 x3 y3]]
  (let [rad (/ (* angle Math/PI) 180)
        sin (Math/sin rad)
        cos (Math/cos rad)]
    [(- (* x1 cos) (* y1 sin))
     (+ (* x1 sin) (* y1 cos))
     (- (* x2 cos) (* y2 sin))
     (+ (* x2 sin) (* y2 cos))
     (- (* x3 cos) (* y3 sin))
     (+ (* x3 sin) (* y3 cos))]))

(defn center-tri [[cx cy] [x1 y1 x2 y2 x3 y3]] 
  [(+ x1 cx)
   (+ y1 cy)
   (+ x2 cx)
   (+ y2 cy)
   (+ x3 cx)
   (+ y3 cy)])

;; Quil fns
(defn setup []
  (no-stroke)
  (color-mode :hsb 256)
  (when STROKE?
    (stroke 0 0 0)
    (stroke-weight 1))
  (background 0 0 0)
  (smooth)
  (frame-rate FPS))

(defn draw-circle! [c]
  (let [{:keys [color center size]} c
        [x y] center]
    ;(stroke 0 0 0 (min size 255))
    (apply fill (concat color [OPACITY]))
    (ellipse x y size size)))

(defn draw-tri! [t]
  (let [{:keys [color center size angle]} t]
    (apply fill (concat color [OPACITY]))
    (apply triangle (center-tri center (rotate-tri angle (tri-points size))))))

(defn draw-shape! [s]
  (if (contains? s :angle)
    (draw-tri! s)
    (draw-circle! s))) 

(defn draw-bg! []
  (no-stroke)
  (fill 0 0 0)
  (rect 0 0 WIDTH HEIGHT))

(defn update []
  (println (count @to-draw))
  (dorun (map draw-shape! @to-draw))
  (swap! to-draw grow-shapes)
  (swap! to-draw remove-large-shapes)
  (when (new-shape?)
    (swap! current-center move-center)
    (swap! to-draw #(concat % [(new-shape)])))
  (when (= @next-direction-frame @frame-num)
    (reset! current-velocity [(+- (rand-int 4)) (+- (rand-int 4))])
    (reset! next-direction-frame (+ @frame-num (brand-int 90 450))))
  (swap! frame-num inc)) 

(defn -main []
  (defsketch grow
    :title "grow"
    :setup setup
    :draw update 
    :size [WIDTH HEIGHT]))
