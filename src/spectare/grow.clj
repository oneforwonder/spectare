(ns spectare.grow
  (:use spectare.util
        quil.core)) 

(def WIDTH (min (screen-width) 1920))
(def HEIGHT (screen-height))

(def FPS 60)

(def SHAPE-TYPES [:tri])
(def START-SIZE 1)
(def ROTATION-FACTOR 2)
(def GROWTH-STYLE :mixed)    ; Either :add, :mult, or :mixed
(def GROWTH-ADD-AMOUNT 2)    ; Used with :add
(def GROWTH-MULT-RATIO 1.03) ; Used with :mult
(def FRAMES-BETWEEN-NEW 4)
(def STROKE true)
(def OPACITY 60)

(def frame-num (atom 0))
(def to-draw (atom []))

;; Shapes
(defn modify-color-comp [cc]
  (-> cc
      (+ (+- (brand-int 30 60)))
      (max 40)     ;; No dark colors, please
      (min 235)))  ;; But also not too bright, please

(defn make-circle 
  ([] (make-circle (rand-color)))
  ([color] {:shape :circle
            :color (map modify-color-comp color)
            :size  START-SIZE})) ; diameter

(defn angle []
  (let [num @frame-num
        alt (= 1 (mod (quot num 120) 2))
        base (if alt (- num) num)]
    (* ROTATION-FACTOR base)))

(defn make-tri
  ([] (make-tri (rand-color)))
  ([color] {:shape :tri
            :color (map modify-color-comp color)
            :size  START-SIZE  ; height
            :angle (angle)}))

(defn grow-shape [s]
  (let [size (:size s)]
    (println "size:" size)
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
  (= 0 (mod @frame-num FRAMES-BETWEEN-NEW)))

(defn new-shape []
  (let [prev-color (or (:color (last @to-draw)) (rand-color))
        shape      (rand-nth SHAPE-TYPES)
        shape-fn   ({:circle make-circle :tri make-tri} shape)]
    (shape-fn prev-color)))

;; Geometry
(defn tri-points [height]
  [0
   (- (/ height 2))
   (- (/ height 1.732))
   (+ (/ height 2))
   (+ (/ height 1.732))
   (+ (/ height 2))])

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

(defn center-tri [[x1 y1 x2 y2 x3 y3]] 
  [(+ x1 (/ WIDTH 2))
   (+ y1 (/ HEIGHT 2))
   (+ x2 (/ WIDTH 2))
   (+ y2 (/ HEIGHT 2))
   (+ x3 (/ WIDTH 2))
   (+ y3 (/ HEIGHT 2))])

;; Quil fns
(defn setup []
  (no-stroke)
  (when STROKE
    (stroke 0 0 0)
    (stroke-weight 1))
  (background 0 0 0)
  (smooth)
  (frame-rate FPS))

(defn draw-circle! [c]
  (let [{:keys [color size]} c]
    (apply fill (concat color [OPACITY]))
    (ellipse (/ WIDTH 2) (/ HEIGHT 2) size size)))

(defn draw-tri! [t]
  (let [{:keys [color size angle]} t]
    (apply fill (concat color [OPACITY]))
    (apply triangle (center-tri (rotate-tri angle (tri-points size))))))

(defn draw-shape! [s]
  (if (contains? s :angle)
    (draw-tri! s)
    (draw-circle! s))) 

(defn draw-bg! []
  (no-stroke)
  (fill 0 0 0)
  (rect 0 0 WIDTH HEIGHT))

(defn update []
  (dorun (map draw-shape! @to-draw))
  (swap! to-draw grow-shapes)
  (swap! to-draw remove-large-shapes)
  (when (new-shape?)
    (swap! to-draw #(concat % [(new-shape)])))
  (swap! frame-num inc)) 

(defn -main []
  (defsketch grow
    :title "grow"
    :setup setup
    :draw update 
    :size [WIDTH HEIGHT]))
