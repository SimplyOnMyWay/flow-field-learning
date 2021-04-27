(ns flow-test.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(def res-factor 0.025)
(def num-steps-global 50)
(def step-length-global 20)

(defn screen-offsets
  "set left-x, right-x, top-y and bottom-y"
  []
  {:left-x (* -0.5 (q/width))
   :right-x (* 1.5 (q/width))
   :top-y (* -0.5 (q/height))
   :bottom-y (* 1.5 (q/height))})

(defn unify-x-y-angle
  [x y angle]
  {:x x :y y :angle angle})

(defn setup-grid
  []
  (let [resolution (* (q/width) res-factor)
        left-x (:left-x (screen-offsets))
        right-x (:right-x (screen-offsets))
        top-y (:top-y (screen-offsets))
        bottom-y (:bottom-y (screen-offsets))
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


(defn get-angle
  [x y state]
  (:angle (first (filter #(and (= (:x %) x) (= (:y %) y)) state))))

(defn update-grid-point
  "leave :x and :y as they are, and adjust :angle"
  [grid-point]
  (let [resolution (* (q/width) res-factor)
        left-x (:left-x (screen-offsets))
        right-x (:right-x (screen-offsets))
        top-y (:top-y (screen-offsets))
        bottom-y (:bottom-y (screen-offsets))
        num-columns (/ (- right-x left-x) resolution)
        num-rows (/ (- bottom-y top-y) resolution)] 
    {:x (:x grid-point)
     :y (:y grid-point)
     :angle (* (/ (/ (:y grid-point) (* (q/width) res-factor)) num-rows) q/PI)}))

(defn update-state [state]
  (map update-grid-point state))

(defn setup []
  (q/frame-rate 30)
  (q/color-mode :hsb 360 100 100 1.0)
  (map update-grid-point (setup-grid)))

(defn x-offset
  [x]
  (- x (:left-x (screen-offsets))))

(defn y-offset
  [y]
  (- y (:left-y (screen-offsets))))

(defn draw-curve-recursive
  "use recur to implement curve"
  [grid x-start y-start num-steps step-length]
  (let [resolution (* (q/width) res-factor)
        p1 {:x x-start :y y-start}
        n num-steps
        ;grid-angle q/QUARTER-PI
        grid-angle (:angle (first (filter #(and (<= (q/abs (- (:x %) (:x p1))) resolution) (<= (q/abs (- (:y %) (:y p1))) resolution)) grid)))
        x-step (* step-length (q/cos grid-angle))
        y-step (* step-length (q/sin grid-angle))
        p2 {:x (+ (:x p1) x-step) :y (+ (:y p1) y-step)}]
    (q/with-stroke [0 100 100 1.0]
      (q/stroke-weight 3)
      (q/line (:x p1) (:y p1) (:x p2) (:y p2)))))


(comment
  (def g (setup-grid))
  (map update-grid-point g)
  (defn check-angle
    []
    (let [resolution (* 900 res-factor)
          p1 {:x 200 :y 400}]
      (:angle (first (filter #(and (<= (q/abs (- (:x %) (:x p1))) resolution) (<= (q/abs (- (:y %) (:y p1))) resolution)) g))))))



(comment
  (defn draw-curve
    "draw curve through point at angle a for a distance d"
    [x-start y-start]
    (q/begin-shape)
    (doseq [n (range num-steps)]
      (let [x-offset ()])
      (q/curve-vertex x-start y-start)
      
      (q/curve-vertex x-start y-start)
      (q/curve-vertex 100 80)
      (q/curve-vertex 100 80))
    (q/end-shape)))


(defn draw-state [state]
  ;(q/no-loop)
  (q/background 50 80 80 0.7)
  (q/with-fill nil
    (q/with-stroke [200 0 100 1.0]
      (draw-grid-angle state)
      (draw-curve-recursive state 800 600 num-steps-global step-length-global))))

(q/defsketch flow-test
  :title "flow-test!"
  :size [900 700]
  :setup setup
  ;:update update-state
  :draw draw-state
  :features [:keep-on-top]
  :middleware [m/fun-mode])
