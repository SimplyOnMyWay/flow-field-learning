(ns flow-test.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(def res-factor 0.01)

(defn unify-x-y-angle
  [x y angle]
  {:x x :y y :angle angle})

(defn setup-grid
  []
  (let [resolution (* (q/width) 0.025)
        left-x (* -0.5 (q/width))
        right-x (* 1.5 (q/width))
        top-y (* -0.5 (q/height))
        bottom-y (* 1.5 (q/height))
        num-columns (/ (- right-x left-x) resolution)
        num-rows (/ (- bottom-y top-y) resolution)
        default-angle q/PI
        x (into [](range left-x right-x resolution)) ; vector apparently more efficient for lookup using get, and lists equivalent is (nth)
        y (into [] (range top-y bottom-y resolution)) ;vector doesn't matter here possibly, but may be more efficient
        xx (apply concat  (for [i (range (count x))] (repeat (count y) (get x i))))
        yy (apply concat (repeat (count x) y))
        angle-vec (repeat (count xx) default-angle)]
    (map unify-x-y-angle xx yy angle-vec)))

(defn get-grid-data
  "get grid 'value' at x y for key k"
  [grid-coll x y k]
  (k (filter #(and (= (:x %) x) (= (:y %) y)) grid-coll)))

(defn draw-grid-angle
  [grid]
  (let [line-length 10]
    (q/with-fill nil
      (q/with-stroke [110 90 50 0.9]
        (doseq [elem-no (range (count grid))]
          (let [elem (nth grid elem-no)
                x (:x elem)
                y (:y elem)
                ;angle (:angle elem)
                angle (* (/ (/ y (* (q/width) res-factor)) (q/sqrt (count grid))) q/PI)]
            (q/ellipse x y 3 3)
            (q/line x y (+ x (* line-length (q/cos angle))) (+ y (* line-length (q/sin angle))))))))))

(defn setup []
  (q/frame-rate 30)
  (q/color-mode :hsb 360 100 100 1.0)
  (setup-grid))

(defn update-grid-point
  "leave :x and :y as they are, and adjust :angle"
  [grid-point]
  (let [angle-inc 0.014] 
    {:x (:x grid-point)
     :y (:y grid-point)
     :angle (+ angle-inc (:angle grid-point))}))

(defn update-state [state]
  (map update-grid-point state))

(defn draw-state [state]
  (q/no-loop)
  (q/background 50 80 80 0.7)
  (q/with-fill nil
    (q/with-stroke [200 0 100 1.0]
      (draw-grid-angle state)
      ;(map update-grid-point state)
      )))

(q/defsketch flow-test
  :title "flow-test!"
  :size [900 700]
  :setup setup
  ;:update update-state
  :draw draw-state
  :features [:keep-on-top]
  :middleware [m/fun-mode])
