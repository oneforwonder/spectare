(ns spectare.grow
  (:use spectare.util
        quil.core)) 

(def WIDTH (min (screen-width) 1920))
(def HEIGHT (screen-height))

(def FPS 60)

(def START-SIZE 10)

(def frame-num (atom 0))
(def to-draw (atom []))

;; Shapes
(defn make-circle []
  {:color  (rand-color)
   :size START-SIZE})

(defn grow-circle [c]
  (assoc c :size ;(+ 4 (:size c))
                 (* 1.03 (:size c))
         ))

;; Quil fns
(defn setup []
  (background 0 0 0)
  (smooth)
  (frame-rate FPS))

(defn draw-circle! [c]
  (let [{:keys [color size]} c]
    (no-stroke)
    (apply fill color)
    (ellipse (/ WIDTH 2) (/ HEIGHT 2) size size)))

(defn draw-bg! []
  (no-stroke)
  (fill 0 0 0)
  (rect 0 0 WIDTH HEIGHT))

(defn update []
  (println (count @to-draw))
  (doseq [c @to-draw] 
    (draw-circle! c))
  (swap! to-draw #(map grow-circle %))
  (swap! to-draw (fn [shapes] (remove #(> (:size %) WIDTH) shapes)))
  (when (= 0 (mod @frame-num 4))
    (swap! to-draw #(concat % [(make-circle)])) )
  (swap! frame-num inc)) 

(defn -main []
  (defsketch grow
    :title "grow"
    :setup setup
    :draw update 
    :size [WIDTH HEIGHT]))
