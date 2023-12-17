(defpackage :graphene-test
  (:use :fiveam :iterate :common-lisp)
  (:export #:run!
           #:graphene-suite
           #:graphene-box
           #:graphene-euler
           #:graphene-frustum
           #:graphene-matrix
           #:graphene-plane
           #:graphene-point
           #:graphene-point3d
           #:graphene-quad
           #:graphene-quaternion
           #:graphene-ray
           #:graphene-rectangle
           #:graphene-size
           #:graphene-sphere
           #:graphene-triangle
           #:graphene-vector))

(in-package :graphene-test)

;; See https://www.embeddeduse.com/2019/08/26/qt-compare-two-floats/
(defun approx-equal (x y &optional (epsilon 1.0e-5))
  (or (< (abs (- x y)) epsilon)
      (< (abs (- x y)) (* epsilon (max (abs x) (abs y))))))

(def-suite graphene-suite)
(in-suite graphene-suite)

;;; --- 2023-12-5 --------------------------------------------------------------
