(ns spectare.geometry)

;; Triangles
(defn centered-equilateral-tri 
  "Given the height of an equilateral triangle, generates the coordinates for
  such a triangle centered on the origin."
  [height]
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

;; Squares
(defn centered-square
  "Given the length of the side of square, generates the coordinates for
  such a square centered on the origin."
  [side]
  (let [hs (/ side 2)]
    [(- hs)    ; x1 (left)
     (- hs)    ; y1 (top)
     (+ hs)    ; x2 (right)
     (- hs)    ; y2 (top)
     (+ hs)    ; x3 (right)
     (+ hs)    ; y3 (bottom)
     (- hs)    ; x4 (left)
     (+ hs)])) ; y4 (bottom)

(defn rotate-square [angle [x1 y1 x2 y2 x3 y3 x4 y4]]
  (let [rad (/ (* angle Math/PI) 180)
        sin (Math/sin rad)
        cos (Math/cos rad)]
    [(- (* x1 cos) (* y1 sin))
     (+ (* x1 sin) (* y1 cos))
     (- (* x2 cos) (* y2 sin))
     (+ (* x2 sin) (* y2 cos))
     (- (* x3 cos) (* y3 sin))
     (+ (* x3 sin) (* y3 cos))
     (- (* x4 cos) (* y4 sin))
     (+ (* x4 sin) (* y4 cos))]))

(defn center-square [[cx cy] [x1 y1 x2 y2 x3 y3 x4 y4]] 
  [(+ x1 cx)
   (+ y1 cy)
   (+ x2 cx)
   (+ y2 cy)
   (+ x3 cx)
   (+ y3 cy)
   (+ x4 cx)
   (+ y4 cy)])

