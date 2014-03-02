(ns spectare.paintdrops
  (:use quil.core)) 

;(def WIDTH 600)
;(def HEIGHT 400)
(def WIDTH (screen-width))
(def HEIGHT (screen-height))

(def RPM 30)  ; avg raindrops per minute
(def FPS 30)
(def OPACITY 120)
(def BG-COLOR [255 255 255])

(def sample-drop {:x 220 :y 110 :size 34 :col [220 80 40] :height 21})
(def falling-drops (atom []))
(def fallen-drops (atom []))
(def frame-num (atom 0))

;; Color manipulation
(defn shade [color percent]
  (map (fn [c] (-> c (+ (* 255 percent)) (Math/round) (max 0) (min 255))) color))

(defn lighten [color percent]
  (shade color percent))

(defn darken [color percent]
  (shade color (- percent)))

;; Splatter
(defn +- [& xs]
  (apply (rand-nth [+ -]) xs))

(defn divvy [total]
  (loop [left total sizes []]
    (if (> left 1.0)
      (let [s (min (* total 0.3) (inc (rand-int left)))]
        (recur (- left s) (conj sizes s)))
      sizes)))

(defn splatter [{:keys [x y size col height] :as parent-drop}]
  (let [splat-sizes (divvy size)]
    (map (fn [s]
           (let [hs (/ size 6.0)]
             {:x (+- x hs (rand-int (* 2 hs)))
              :y (+- y hs (rand-int (* 2 hs)))
              :size (+ 5 s)
              :col (darken col (+ (rand 0.1)))
              :height 0})) splat-sizes)))

(defn splat [d]
  (concat [d] (when (= 0 (:height d)) (splatter d))))

;; Drop fns
(defn new-drop []
  {:x (rand-int WIDTH)
   :y (rand-int HEIGHT)
   :size (+ 88 (rand-int (/ HEIGHT 50)))
   :col (repeatedly 3 #(rand-int 256))
   :height 21})

(defn new-drops []
  ;; Should use RPM and FPS. Currently does 1 drop per frame.
  [(new-drop)])

(defn update-drop [{:keys [height col size] :as d}]
  (when (not= 0 height)
    (assoc d :height (dec height)
             :size (* size (if (= height 1) 2.2 0.94))
             :col (lighten col 0.002))))

(defn update-drops [drops]
  (->> drops
       (map update-drop)
       (mapcat splat)
       (concat (new-drops))
       (filter identity)))

(defn get-fallen []
  (filter #(= 0 (:height %)) @falling-drops))

;; Quil fns
(defn setup []
  (smooth)
  (frame-rate FPS))

(defn draw-drops! [drops]
  (doseq [{:keys [x y size col]} drops]
    (no-stroke)
    (apply fill (concat col [OPACITY]))
    (ellipse x y size size)))

(defn draw-bg! []
  (no-stroke)
  (apply fill BG-COLOR)
  (rect 0 0 WIDTH HEIGHT))

(defn update []
  (draw-bg!)
  (swap! falling-drops update-drops)
  (swap! fallen-drops #(concat % (get-fallen)))
  (swap! frame-num inc)
  (draw-drops! @fallen-drops)
  (draw-drops! @falling-drops))

(defn -main []
  (defsketch raindrops
    :title "Raindrops"
    :setup setup
    :draw update
    :size [WIDTH HEIGHT]))

