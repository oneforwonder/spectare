(ns spectare.geometry)

;; Polygon
(defn center-poly [[cx cy] points]
  (let [xs (take-nth 2 points) 
        ys (take-nth 2 (drop 1 points))]
    (interleave (map #(+ cx %) xs) (map #(+ cy %) ys))))

(defn rotate-poly [degrees points]
  (let [pairs (partition 2 points) 
        rad   (/ (* degrees Math/PI) 180)
        sin   (Math/sin rad)
        cos   (Math/cos rad)]
    (mapcat (fn [[x y]] 
              [(- (* x cos) (* y sin))
               (+ (* x sin) (* y cos))]) pairs)))

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

