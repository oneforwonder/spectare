(ns spectare.branching
  (:require [spectare.geometry :as geo])
  (:use spectare.util
        quil.core)) 

(def WIDTH (min (screen-width) 1920))
(def HEIGHT (screen-height))

(def ROOT-COUNT 8)
(def ROOT-LENGTH 128)
(def ROOT-WEIGHT 8)
(def ROOT-COLOR [0 0 255])

(def NUM-GENS 11)
(def CHILD-COUNT 2)
(def LENGTH-RATIO (/ 3 4))
(def WEIGHT-RATIO (/ 3 4))

(def FPS 1)

(def gens (atom nil))
(def gen-num (atom 0))

;; Lines
(defn rand-pos []
  [(rand-int WIDTH) (rand-int HEIGHT)])

(defn make-line [weight length [cx cy] degrees]
  (let [[dx dy] (geo/rotate-point degrees [length 0])]
    {:x1 cx
     :y1 cy
     :x2 (+ cx dx)
     :y2 (+ cy dy)
     :angle  degrees   
     :length length
     :weight weight}))

(defn make-roots []
  (let [center [(/ WIDTH 2) (/ HEIGHT 2)]
        angles (range 0 360 (/ 360 ROOT-COUNT))]
    (map #(make-line ROOT-WEIGHT ROOT-LENGTH center %) angles)))

(defn make-children [parent angles]
  (let [{:keys [angle weight length x2 y2]} parent]
    (map #(make-line (* weight WEIGHT-RATIO) (* length LENGTH-RATIO) [x2 y2] (+ angle %)) angles)))

(defn rand-tree []
  (let [angles (repeatedly 2 #(brand-int -180 181))]
    (iterate (fn [gen] (mapcat #(make-children % angles) gen)) (make-roots))))

;; Quil fns
(defn setup []
  (reset! gens (rand-tree))
  (color-mode :hsb 256)
  (background 0 0 0)
  (smooth)
  (frame-rate FPS))

(defn draw-line! [{:keys [x1 y1 x2 y2 weight]}] 
  (stroke-weight 1)
  (apply stroke ROOT-COLOR)
  (line x1 y1 x2 y2))

(defn draw-bg! []
  (no-stroke)
  (fill 0 0 0 40)
  (rect 0 0 WIDTH HEIGHT))

(defn update []
  (draw-bg!)
  (if (< @gen-num NUM-GENS)
    (do 
      (doseq [l (nth @gens @gen-num)]
        (draw-line! l))
      (swap! gen-num inc))
    (do
      (reset! gens (rand-tree))
      (reset! gen-num 0))))

(defn -main []
  (defsketch branching
    :title "branching"
    :setup setup
    :draw update
    :size [WIDTH HEIGHT]))

