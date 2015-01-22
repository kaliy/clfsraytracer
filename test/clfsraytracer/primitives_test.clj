(ns clfsraytracer.primitives-test
  (:require [clojure.test :refer :all]
            [clfsraytracer.primitives :refer :all]))

(defn float= [f1 f2]
  (<= (- f1 f2) 0.001))

(deftest v3d-creates-v3d-struct
  (let [vertice (v3d 666 4 13)]
    (testing "Vertice creation"
      (testing "checking x"
        (is (= 666 (:x vertice))))
      (testing "checking y"
        (is (= 4 (:y vertice))))
      (testing "checking z"
        (is (= 13 (:z vertice)))))))

(deftest substract-v3d
  (let [vertice (subtract (v3d 10 10 10) (v3d 1 2 3))]
    (testing "Substraction of x"
      (is (= 9 (:x vertice))))
    (testing "Substraction of y"
      (is (= 8 (:y vertice))))
    (testing "Substraction of z"
      (is (= 7 (:z vertice))))))

(deftest dot-product-test
  (let [v1 (v3d 1 2 3)
        v2 (v3d 2 3 4)
        v0 (v3d 0 0 0)]
    (testing "Dot product calculation"
      (testing "of zero"
        (is (= 0 (dot-product v1 v0)))
        (is (= 0 (dot-product v0 v2))))
      (testing "of normal vectors"
        (is (= 20 (dot-product v1 v2)))
        (is (= 20 (dot-product v2 v1)))))))

(deftest cross-product-test
  (let [v1 (v3d 1 2 3)
        v2 (v3d 2 3 4)
        v0 (v3d 0 0 0)]
    (testing "Cross product calculation"
      (testing "vector 1 and 2"
        (is (= (v3d -1 2 -1) (cross-product v1 v2)))
        (is (= (v3d 1 -2 1) (cross-product v2 v1))))
      (testing "zero vector"
        (is (= (v3d 0 0 0) (cross-product v1 v0)))
        (is (= (v3d 0 0 0) (cross-product v0 v1)))
        (is (= (v3d 0 0 0) (cross-product v2 v0)))
        (is (= (v3d 0 0 0) (cross-product v0 v2)))))))

(deftest normalize-vector-test
  (let [norm (normalize (v3d 10 20 30))]
    (testing "Vector normalization"
      ;0,267261241912, 0,534522483825, 0,801783725737
      (is (float= 0.267261 (:x norm)))
      (is (float= 0.534522 (:y norm)))
      (is (float= 0.801783 (:z norm))))))

(deftest magnitude-test
  (testing "Magnitude calculation"
    (testing "for floats"
      (is (float= 3.741657 (magnitude (v3d 1.0 2.0 3.0)))))
    (testing "for ints"
      (is (float= 3.741657 (magnitude (v3d 1 2 3)))))))

(deftest get-ray-face-intersection-test
  (let [face1 (face (v3d 10.0 0 0) (v3d 0 0 0) (v3d 0 10.0 0))]
    (testing "Intersection testing"
      (let [intersection (get-ray-face-intersection (ray (v3d 5.0 5.0 3.0) (v3d -1.0 -1.0 -1.0)) face1)]
        (is (= (v3d 2.0 2.0 0) intersection))))))