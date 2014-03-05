(ns spectare.test
  (:use [spectare.core :exclude [-main]]
        [clojure.pprint :only [pprint]]))

(defmacro pev [form]
  `(let [x# ~form]
     (println '~form '~'->)
     (pprint x#)
     x#))

(defn color-manipulation []
  (pev (lighten [120 130 140] 0.1))
  (pev (lighten [120 130 140] 0.2))
  (pev (lighten [0 0 0] 0.1))
  (pev (lighten [255 255 255] 0.1))
  (println)

  (pev (darken [120 130 140] 0.1))
  (pev (darken [120 130 140] 0.2))
  (pev (darken [0 0 0] 0.1))
  (pev (darken [255 255 255] 0.1))
  (println))

(defn div []
  (pev (divvy 40))
  (pev (divvy 20))
  (pev (divvy 10))
  (pev (divvy 2))
  (pev (divvy 1)))

(defn -+ []
  (pev (+- 2))
  (pev (+- 2))
  (pev (+- 2))
  (pev (+- 12 17 34 16)))

(defn spltr []
  (pev (splatter {:x 220 :y 110 :size 34 :col [220 80 40] :height 21})))

(defn splt []
  (pev (splat {:x 220 :y 110 :size 34 :col [220 80 40] :height 21}))
  (pev (splat {:x 220 :y 110 :size 34 :col [220 80 40] :height 0}))) 

(defn new-drop []
  (pev (new-drops))
  (pev (new-drops))
  (pev (new-drops))
  (pev (new-drops)))

(defn update-d []
  (pev (update-drop {:x 220 :y 110 :size 34 :col [220 80 40] :height 21})) 
  (pev (update-drop {:x 220 :y 110 :size 34 :col [220 80 40] :height 0})))

(defn update-ds []
  (pev (update-drops [{:x 220 :y 110 :size 34 :col [220 80 40] :height 21}])) 
  (pev (update-drops [{:x 220 :y 110 :size 34 :col [220 80 40] :height 1}])) 
  (pev (update-drops [{:x 220 :y 110 :size 34 :col [220 80 40] :height 0}])))

(defn -main []
  ;(color-manipulation)
  ;(div)
  ;(-+)
  ;(spltr)
  (splt)
  ;(new-drop)
  ;(update-d) 
  ;(update-ds) 
  nil) 
