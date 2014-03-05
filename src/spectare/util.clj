(ns spectare.util)

;; A little sugar
(defn map! 
  "map which allows for side-effecting functions."
  [& args]
  (dorun (apply map args)))

;; Random fns
(defn brand-int 
  "Returns a bounded random integer in range [x,y)"
  [x y]
  (+ x (rand-int (- y x))))

(defn +- 
  ([]  (rand-nth [+ -]))
  ([x] ((+-) x)))

;; Color fns
(defn rand-rgb-color []
  (repeatedly 3 #(rand-int 256)))

(defn rand-rgba-color []
  (repeatedly 4 #(rand-int 256)))

(defn rand-hsb-color []
  (repeatedly 3 #(rand-int 256)))

(defn rand-hsba-color []
  (repeatedly 4 #(rand-int 256)))

(defn rand-vivid-hsb-color 
  "Generates a HSB color of random hue, full saturation, and high brightness.
  The fixed saturation and brightness were chosen to make vivid, pretty colors."
  []
  [(rand-int 256) 255 200])

(defn shift-hsb-hue 
  "Given a HSB color, creates a color of similar (but not equal) hue.
  This function can shift the hue forward or backwards through the hue spectrum
  but is weighted towards moving forward, so that repeated calls to the this
  function will create a variety of colors, not stay in the same place."
  ;; TODO: Parameterize how much to shift
  [color]
  (-> (first color)                 ; hue component
      (+ (+- (brand-int 10 30)) 5)  ; +5 weights fn towards moving forward
      (mod 256)
      (vector 255 180)))

(defn gradient 
  "percent=0.0: c1; percent=1.0: c2; 0.0 < percent < 1.0: a blend"
  [c1 c2 percent]
  (map (fn [cc1 cc2] (+ 0.0 cc1 (* percent (- cc2 cc1)))) c1 c2))

;; Misc fns
(defn indexed
    "Returns a lazy sequence of [index, item] pairs, where items come
    from 's' and indexes count up from zero.
      (indexed '(a b c d))  =>  ([0 a] [1 b] [2 c] [3 d])"
    [s]
    (map vector  (iterate inc 0) s))
