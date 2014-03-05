(ns spectare.grow
  (:require [spectare.geometry :as geo])
  (:use spectare.util
        quil.core)
  (:gen-class)) 

;; FPS
(def FPS 30)

;; Size
(def WIDTH (min (screen-width) 1920))
(def HEIGHT (screen-height))
(def SCREEN-CENTER [(/ WIDTH 2) (/ HEIGHT 2)])

;; Background
(def BG-COLOR [0 0 0])
(def DRAW-BG? true)            ; Re-draw BG between frames, clearing screen
(def ALTERNATE-BG? false)      ; Alternate BG color between black and white 

;; Shapes
(def SHAPES [:square :tri :circle]) ; Valid shapes are :tri :circle and :square
(def CHANGE-SHAPES? true)           ; TODO: Implement. Currently always changes.
(def SHAPE-TIME (* 6 FPS))          ; How long, in frames, between changing shapes

;; Stroke, fill, opacity
(def OUTLINE? true)            ; Whether to draw 1px outline around shapes
(def OUTLINE-COLOR [0 0 0])
(def FILL? false)              ; When active, shapes are filled and outline can be used
(def SIZE-TO-STROKE 40)        ; If fill is not active, color is moved to thick strokes
(def OPACITY 255)

;; New shapes
(def START-SIZE 1)
(def FRAMES-BETWEEN-NEW 5)

;; Growth
(def GROWTH-STYLE :mixed)      ; Either :add :mult or :mixed
(def GROWTH-ADD-AMOUNT 1)      ; Used with :add
(def GROWTH-MULT-RATIO 1.03)   ; Used with :mult

;; Rotation
(def ROTATE? true)
(def ROTATION-FACTOR 1)

;; Center movement
(def MOVE-CENTER? true)
(def VEL-COMP-MAX 0.7)         ; In pixels/frame, for each velocity component
(def VEL-TIME-MIN (* 2 FPS))   ; In frames
(def VEL-TIME-MAX (* 6 FPS))

;; Stateful variables
(def frame-num (atom 0))
(def current-shapes (atom []))
(def current-bg-color (atom [0 0 0]))
(def current-center (atom [(/ WIDTH 2) (/ HEIGHT 2)]))
(def current-velocity (atom [0 0]))
(def next-velocity-frame (atom 0))

;; Environment
(defn move-center [center]
  (let [[cx cy] center
        [vx vy] @current-velocity]
    [(-> (+ cx vx) (max 0) (min WIDTH))
     (-> (+ cy vy) (max 0) (min HEIGHT))]))

(defn rand-center-velocity []
  [(+- (rand VEL-COMP-MAX)) (+- (rand VEL-COMP-MAX))])

;; Shapes
(defn shift-hsb-hue 
  "Given a HSB color, creates a color of similar (but not equal) hue.
  This function can shift the hue forward or backwards through the hue spectrum
  but is weighted towards moving forward, so that repeated calls to the this
  function will create a variety of colors, not stay in the same place."
  [color]
  (-> (first color)                 ; hue component
      (+ (+- (brand-int 10 30)) 5)  ; +5 weights fn towards moving forward
      (mod 256)
      (vector 255 180)))

(defn make-circle
  [{:keys [center color size]}]
  {:shape  :circle
   :center (or center SCREEN-CENTER)
   :color  (or color (rand-vivid-hsb-color))
   :size   (or size START-SIZE)}) ; diameter

(defn make-tri
  [{:keys [center color size angle]}]
  {:shape  :tri
   :center (or center SCREEN-CENTER)
   :color  (or color (rand-vivid-hsb-color))
   :size   (or size START-SIZE) ; height
   :angle  (or angle 0)})

(defn make-square
  [{:keys [center color size angle]}]
  {:shape  :square
   :center (or center SCREEN-CENTER)
   :color  (or color (rand-vivid-hsb-color))
   :size   (or size START-SIZE) ; length of side
   :angle  (or angle 0)})

(defn grow-shape [s]
  (let [size (:size s)]
    (assoc s :size (case GROWTH-STYLE
                     :add   (+ size GROWTH-ADD-AMOUNT)
                     :mult  (* size GROWTH-MULT-RATIO)
                     :mixed (max (+ size GROWTH-ADD-AMOUNT) 
                                 (* size GROWTH-MULT-RATIO)))))) 

(defn grow-shapes [ss]
  (map grow-shape ss))

(defn remove-large-shapes [ss]
  (remove (fn [s]
            (case (:shape s)
              :circle (> (:size s) (* 1.6 WIDTH))
              :tri    (> (:size s) (* 2.8 WIDTH))
              :square (> (:size s) (* 1.6 WIDTH)))) ss))

;; Stateful functions
(defn angle []
  (if ROTATE?
    (* ROTATION-FACTOR @frame-num)
    0)) 

(defn new-shape? []
  (and (or (not CHANGE-SHAPES?) (< 12 (mod @frame-num SHAPE-TIME)))
       (= 0 (mod @frame-num FRAMES-BETWEEN-NEW))))

(defn new-shape []
  (let [shape      (nth SHAPES (mod (quot @frame-num SHAPE-TIME) (count SHAPES)))
        shape-fn   ({:circle make-circle :tri make-tri :square make-square} shape)
        prev-color (or (:color (last @current-shapes)) (rand-vivid-hsb-color)) 
        options    {:color (shift-hsb-hue prev-color)
                    :center @current-center
                    :angle (angle)}]
    (shape-fn options)))

(defn new-velocity? []
  (= @next-velocity-frame @frame-num))

;; Quil fns
(defn setup []
  (no-stroke)
  (no-fill)
  (smooth)
  (color-mode :hsb 256)
  (apply background BG-COLOR)
  (frame-rate FPS))

(defn set-fill-and-stroke! [color size]
  (if OUTLINE?
    (do (stroke-weight 2)
        (apply stroke OUTLINE-COLOR))
    (no-stroke))

  (if FILL? 
    (apply fill (concat color [OPACITY]))
    (do (no-fill)
        (apply stroke (concat color [OPACITY]))
        (stroke-weight (max 2 (/ size SIZE-TO-STROKE))))))

(defn draw-circle! [c]
  (let [{:keys [color center size]} c
        [cx cy] center]
    (set-fill-and-stroke! color size)
    (ellipse cx cy size size)))

(defn draw-tri! [t]
  (let [{:keys [color center size angle]} t]
    (set-fill-and-stroke! color size)
    (apply triangle (->> (geo/centered-equilateral-tri size)
                         (geo/rotate-poly angle)
                         (geo/center-poly center)))))

(defn draw-square! [s]
  (let [{:keys [color center size angle]} s
        [cx cy] center]
    (set-fill-and-stroke! color size)
    (apply quad (->> (geo/centered-square size)
                     (geo/rotate-poly angle)
                     (geo/center-poly center)))))

(defn draw-shape! [s]
  (case (:shape s)
    :circle (draw-circle! s)
    :tri    (draw-tri! s)
    :square (draw-square! s)))

(defn draw-bg! []
  (no-stroke)
  (apply fill @current-bg-color)
  (rect 0 0 WIDTH HEIGHT))

(defn update []
  ;; Drawing
  (when DRAW-BG? (draw-bg!))
  (let [shapes  @current-shapes
        ordered (if FILL? shapes (reverse shapes))]
    (map! draw-shape! ordered))

  ;; Update shapes
  (swap! current-shapes grow-shapes)
  (swap! current-shapes remove-large-shapes)
  (when (new-shape?)
    (swap! current-shapes #(concat % [(new-shape)])))

  ;; Update environment
  (when MOVE-CENTER?
    (swap! current-center move-center)
    (when (new-velocity?) 
      (reset! current-velocity (rand-center-velocity))
      (reset! next-velocity-frame (+ @frame-num (brand-int VEL-TIME-MIN VEL-TIME-MAX)))))
  (when ALTERNATE-BG? 
    (reset! current-bg-color [0 0 (* 255 (Math/sin (/ @frame-num 50)))]))
  (swap! frame-num inc)) 

(defn -main []
  (defsketch grow
    :title "grow"
    :setup setup
    :draw update
    :size [WIDTH HEIGHT]))
