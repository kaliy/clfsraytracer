(ns clfsraytracer.primitives
  (:require [clfsraytracer.utils :refer :all]))


(defstruct v3d-struct :x :y :z)
(defn v3d [x y z]
  "Vertice with 3 coordinates"
  (struct v3d-struct x y z))

(defstruct face-struct :p0 :p1 :p2)
(defn face [p1 p2 p3]
  (struct face-struct p1 p2 p3))

(defstruct ray-struct :origin :direction)
(defn ray [origin direction]
  (struct ray-struct origin direction))

(defstruct object-struct :name :faces)
(defn object [name faces]
  (struct object-struct name faces))

(defn subtract [u v]
  (apply v3d (map #(- %1 %2) (vals u) (vals v))))

(defn dot-product [u v]
  (reduce + (map #(* %1 %2) (vals u) (vals v))))

(defn cross-product [u v]
  (v3d (- (* (:y u) (:z v)) (* (:z u) (:y v)))
       (- (* (:z u) (:x v)) (* (:x u) (:z v)))
       (- (* (:x u) (:y v)) (* (:y u) (:x v)))))

(defn magnitude [u]
  (sqrt (apply + (map sq (vals u)))))

(defn multiply [t u]
  (apply v3d (map #(* % t) (vals u))))

(defn add [u v]
  (apply v3d (map #(+ %1 %2) (vals u) (vals v))))

(defn normalize [u]
  (let [mag (magnitude u)]
    (apply v3d (map #(/ % mag) (vals u)))))

(defn get-ray-face-intersection [r f]
  ;http://goo.gl/RZkys
  (let [E1 (subtract (:p1 f) (:p0 f))
        E2 (subtract (:p2 f) (:p0 f))
        T (subtract (:origin r) (:p0 f))
        Q (cross-product T E1)
        D (:direction r)
        P (cross-product D E2)
        coef (/ 1 (dot-product P E1))
        t (* coef (dot-product Q E2))]
    (if (< t 0) nil
    (let [u (* coef (dot-product P T))
           v (* coef (dot-product Q D))]
      (if (and (>= u 0) (>= v 0) (<= (+ u v) 1))
        (add (:origin r) (multiply t D)))
      nil))))

(defn face-intersect [ray face]
  "Detecting ray and rectangle face intersection by Tomas Möller and Ben Trumbore method: http://goo.gl/RZkys"
  )

