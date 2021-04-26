(ns flow-test.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(defn unify-x-y
  [x y]
  {:x x :y y})

(defn setup-grid
  []
  (let [resolution 10;(* (q/width) 0.01)
        left-x -250 ;(* -0.5 (q/width))
        right-x 750 ;(* 1.5 (q/width))
        top-y -250 ;(* -0.5 (q/height))
        bottom-y 750 ;(* 1.5 (q/height))
        num-columns (/ (- right-x left-x) resolution)
        num-rows (/ (- bottom-y top-y) resolution)
        default-angle 46 ;q/QUARTER-PI
        x (into [](range left-x right-x resolution)) ; vector apparently more efficient for lookup using get, and lists equivalent is (nth)
        y (into [] (range top-y bottom-y resolution)) ;vector doesn't matter here possibly, but may be more efficient
        xx (apply concat  (for [i (range (count x))] (repeat (count y) (get x i))))
        yy (apply concat (repeat (count x) y))]

    (map unify-x-y xx yy)
    
))

(defn get-grid-data
  "get grid 'value' at x y for key k"
  [grid-coll x y k]
  (k (filter #(and (= (:x %) x) (= (:y %) y)) grid-coll)))

(defn draw-grid-angle
  [grid]
  (q/with-fill nil
    (q/with-stroke [110 90 50 0.9]
      (doseq [elem-no (range (count grid))]
        (let [elem (nth grid elem-no)
              x (:x elem)
              y (:y elem)
              angle (:angle elem)]
          (q/ellipse x y 5 5))))))

(defn print-state
  [state]
  (println state))

(defn setup []
  (q/frame-rate 0.5)
  (q/color-mode :hsb 360 100 100 1.0)
  (setup-grid))

(defn update-state [state]
  {:x (:x state)
   :y (:y state)
   :angle (:angle state)})

(defn draw-state [state]
  (q/background 50 80 80 0.7)
  (q/with-fill nil
    (q/with-stroke [200 0 100 1.0]
      (draw-grid-angle state))))

(q/defsketch flow-test
  :title "flow-test!"
  :size [500 500]
  :setup setup
  ;:update update-state
  :draw draw-state
  :features [:keep-on-top]
  :middleware [m/fun-mode])
