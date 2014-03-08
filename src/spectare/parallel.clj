(ns spectare.parallel
  (:use spectare.util
        quil.core)) 

(def WIDTH (min (screen-width) 1920))
(def HEIGHT (screen-height))

(def FPS 60)

(def AVG-LINE-COLL-COUNT 30)
(def LINE-THICKNESS 2)
(def LINES-FADE true)
(def LINES-ON-SCREEN 100) ; Only used if lines fade

(def frame-num (atom 0))

;; Helpers
(defn on-screen? [line]
  (let [{:keys [x1 y1 x2 y2]} line]
    (or (and (< 0 x1 WIDTH)
             (< 0 y1 HEIGHT))
        (and (< 0 x2 WIDTH)
             (< 0 y2 HEIGHT)))))

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
  (let [c1 (rand-rgb-color)
        c2 (rand-rgb-color)
        p1 (rand-pos)
        p2 (rand-pos)
        dt (brand-int 5 10)
        dx (+- (rand-int dt))
        dy (+- (- dt (abs dx)))
        ac (* 0.67 AVG-LINE-COLL-COUNT)
        n  (+ ac (rand-int ac))] ; Ranges from 2/3 avg to 4/3 avg
    (filter on-screen? (map #(make-line c1 c2 p1 p2 dx dy % n) (range n)))))

;; Quil fns
(defn setup []
  (def lines (apply concat (repeatedly make-lines)))
  (background 0 0 0)
  (smooth)
  (frame-rate FPS))

(defn draw-line! [{:keys [x1 y1 x2 y2 col]} alpha] 
  (stroke-weight LINE-THICKNESS)
  (apply stroke (concat col [alpha]))
  (line x1 y1 x2 y2))

(defn draw-bg! []
  (no-stroke)
  (fill 0 0 0)
  (rect 0 0 WIDTH HEIGHT))

(defn update-fading []
  (let [i  (inc @frame-num)
        d  (max 0 (- i LINES-ON-SCREEN))
        ls (drop d (take i lines))
        l* (map vector (reverse ls) (iterate #(- % (/ 255 LINES-ON-SCREEN)) 255))]
    (draw-bg!)
    (doseq [[l a] (reverse l*)]
      (draw-line! l a))
    (swap! frame-num inc)))

(defn update-stacking []
  (draw-line! (nth lines @frame-num) 255)
  (swap! frame-num inc))

(defn -main []
  (defsketch parallel
    :title "parallel"
    :setup setup
    :draw (if LINES-FADE update-fading update-stacking)
    :size [WIDTH HEIGHT]))
