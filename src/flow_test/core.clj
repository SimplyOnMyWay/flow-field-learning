(ns flow-test.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(def res-factor 0.01)
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

(defn find-grid-angle
  "return angle within distance 'resolution' of x and y"
  [x y grid res]
  (:angle (first (filter #(and (<= (q/abs (- (:x %) x)) res) (<= (q/abs (- (:y %) y)) res)) grid))))

(defn draw-curve-recursive
  "use recur to implement curve"
  [grid x-start y-start num-steps step-length]
  (let [resolution (* (q/width) res-factor)]
    (loop [x1 x-start
           y1 y-start
           step-num 0]
      (let [grid-angle (find-grid-angle x1 y1 grid resolution)
            x-step (* step-length (q/cos grid-angle))
            y-step (* step-length (q/sin grid-angle))
            x2 (+ x1 x-step)
            y2 (+ y1 y-step)]
        (when (< step-num num-steps)
          (q/with-stroke [0 100 100 1.0]
            (q/stroke-weight 3)
            (q/line x1 y1 x2 y2))
          (recur x2
                 y2
                 (inc step-num)))))))

(defn draw-state [state]
  (q/no-loop)
  (q/background 50 80 80 0.7)
  (q/with-fill nil
    (q/with-stroke [200 0 100 1.0]
      ;(draw-grid-angle state)
      (draw-curve-recursive state 100 100 num-steps-global step-length-global))))

(q/defsketch flow-test
  :title "flow-test!"
  :size [900 700]
  :setup setup
  ;:update update-state
  :draw draw-state
  :features [:keep-on-top]
  :middleware [m/fun-mode])
