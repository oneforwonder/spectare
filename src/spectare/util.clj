(ns spectare.util)

; Random fns
(defn brand-int 
  "Returns a bounded random integer in range [x,y)"
  [x y]
  (+ x (rand-int (- y x))))

(defn brand-color []
  [(brand-int 80 256)
   (brand-int 0 160)
   (brand-int 40 220)])

(defn rand-color []
  (repeatedly 3 #(rand-int 256)))

(defn +- 
  ([]  (rand-nth [+ -]))
  ([x] ((+-) x)))

;; Color fns
(defn gradient 
  "percent=0.0 -> c1; percent=1.0 -> c2; 0.0 < percent < 1.0 -> a blend"
  [c1 c2 percent]
  (map (fn [cc1 cc2] (+ 0.0 cc1 (* percent (- cc2 cc1)))) c1 c2))

;; Misc fns
(defn indexed
    "Returns a lazy sequence of [index, item] pairs, where items come
    from 's' and indexes count up from zero.
      (indexed '(a b c d))  =>  ([0 a] [1 b] [2 c] [3 d])"
    [s]
    (map vector  (iterate inc 0) s))
