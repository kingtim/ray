(ns ray.core
  (:require [clojure.math.numeric-tower :as math]))

(defn sq [x] (* x x))

(defn mag [x y z]
  (math/sqrt (+ (sq x) (sq y) (sq z))))

(defn unit-vector [x y z]
  (let [d (mag x y z)]
    [(/ x d) (/ y d) (/ z d)]))

(defrecord Point [x y z])

(defn distance [p1 p2]
  (mag (- (:x p1) (:x p2))
       (- (:y p1) (:y p2))
       (- (:z p1) (:z p2))))

(defn minroot [a b c]
  (if (zero? a)
    (/ (- c) b)
    (let [disc (- (sq b) (* 4 a c))]
      (if (pos? disc)
              (let [discrt (math/sqrt disc)]
                (min (/ (+ (- b) discrt) (* 2 a))
                     (/ (- (- b) discrt) (* 2 a))))
              nil))))

(defrecord Surface [color])

(def ^:dynamic *world* nil)

(def eye (Point. 0 0 200))

(defn lambert [s int xr yr zr]
  (let [[xn yn zn] (sphere-normal s int)]
    (max 0 (+ (* xr xn) (* yr yn) (* zr zn)))))

(defn hit [s pt xr yr zr]
  (let [h (sphere-intersect s pt xr yr zr)]
    (when h
      (let [d (distance h pt)]
        [d [s h]]))))

(defn first-hit [pt xr yr zr]
  (let [hits (for [s *world* :let [h (hit s pt xr yr zr)] :when h] h)]
    (if (empty? hits)
      [nil nil]
      (second (apply min-key #(first %) hits)))))

(defn sendray [pt xr yr zr]
  (let [[s int] (first-hit pt xr yr zr)]
    (if s
      (* (lambert s int xr yr zr) (:color (:surface s)))
      0)))

(defn color-at [x y]
  (let [[xr yr zr]
        (unit-vector (- x (:x eye))
                     (- y (:y eye))
                     (- 0 (:z eye)))]
    (math/round (* (sendray eye xr yr zr) 255))))

(defprotocol Shape
  (intersect [this pt xr yr zr])
  (normal [this pt]))

(defn sphere-intersect [s pt xr yr zr]
  (let [c (:center s)
        n (minroot (+ (sq xr) (sq yr) (sq zr))
                   (* 2 (+ (* (- (:x pt) (:x c)) xr)
                           (* (- (:y pt) (:y c)) yr)
                           (* (- (:z pt) (:z c)) zr)))
                   (+ (sq (- (:x pt) (:x c)))
                      (sq (- (:y pt) (:y c)))
                      (sq (- (:z pt) (:z c)))
                      (- (sq (:radius s)))))]
    (if n
      (Point. (+ (:x pt) (* n xr))
              (+ (:y pt) (* n yr))
              (+ (:z pt) (* n zr))))))


(defn sphere-normal [s pt]
  (let [c (:center s)]
    (unit-vector (- (:x c) (:x pt))
                 (- (:y c) (:y pt))
                 (- (:z c) (:z pt)))))

(defprotocol Shape
  (intersect [this pt xr yr zr])
  (normal [this pt]))

(defrecord Sphere [radius center surface] Shape
           (intersect [this pt xr yr zr]
             (sphere-intersect this pt xr yr zr))
           (normal [this pt]
             (sphere-normal this pt)))

(defn defsphere [x y z r c]
  (let [s (Sphere. r (Point. x y z) (Surface. c))]
    (def ^:dynamic *world* (cons s *world*))))

(defn tracer [pathname]
  (with-open [w (java.io.PrintWriter. (java.io.FileWriter. pathname))]
    (.println w (str  "P2 " (* 1 100) " " (* 1 100) " 255"))
    (doseq [y (range -49 51) x (range -49 51)]
      (.print w (str  (color-at x y) " "))
      (when (= 50 x) (.println w)))))

(defn ray-test []
  (def ^:dynamic *world* nil)
  (defsphere 0 -300 -1200 200 0.8)
  (defsphere -80 -150 -1200 200 0.7)
  (defsphere 70 -100 -1200 200 0.9)
  (tracer "/tmp/spheres.pgm"))