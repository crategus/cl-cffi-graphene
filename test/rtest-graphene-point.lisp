(in-package :graphene-test)

(def-suite graphene-point :in graphene-suite)
(in-suite graphene-point)

(export 'graphene-point)

;;; --- Types and Values -------------------------------------------------------

;;;     graphene_point_t

;;; --- Macros -----------------------------------------------------------------

;;;     with-graphene-point

(test with-graphene-point.1
  (with-graphene-point (p)
    (is (= 0.0 (point-x p)))
    (is (= 0.0 (point-y p)))))

(test with-graphene-point.2
  (with-graphene-point (p 1 2)
    (is (= 1.0 (point-x p)))
    (is (= 2.0 (point-y p)))))

(test with-graphene-point.3
  (with-graphene-points ((p1 3 4) (p p1))
    (is (= 3.0 (point-x p)))
    (is (= 4.0 (point-y p)))))

(test with-graphene-point.4
  (with-graphene-points ((p1 5 6) (p (p1 point-t)))
    (is (= 5.0 (point-x p)))
    (is (= 6.0 (point-y p)))))

(test with-graphene-point.5
  (with-graphene-vec2 (v 1.5 2.5)
    (with-graphene-point (p (v vec2-t))
      (is (= 1.5 (point-x p)))
      (is (= 2.5 (point-y p))))))

(test with-graphene-point.6
  (let ((a 3.5) (b 4.5))
    (with-graphene-point (p a b)
      (is (= 3.5 (point-x p)))
      (is (= 4.5 (point-y p))))))

;;;     with-graphene-points

(test with-graphene-points.1
  (with-graphene-points ((p1) (p2) (p3))
    (is (point-equal p1 (point-zero)))
    (is (point-equal p2 (point-zero)))
    (is (point-equal p3 (point-zero)))))

(test with-graphene-points.2
  (with-graphene-points (p1 p2 p3)
    (is (point-equal p1 (point-zero)))
    (is (point-equal p2 (point-zero)))
    (is (point-equal p3 (point-zero)))))

(test with-graphene-points.3
  (with-graphene-points (p1 (p2 1 2) (p3 p2) (p4 (p3 point-t)))
    (is (point-equal p1 (point-zero)))
    (is (= 1.0 (point-x p2)))
    (is (= 2.0 (point-y p2)))
    (is (point-equal p3 p2))
    (is (point-equal p4 p3))))

(test with-graphene-points.4
  (with-graphene-vec2 (v 1.5 2.5)
    (with-graphene-points ((p1 (v vec2-t)) p2 (p3 p1))
      (is (= 1.5 (point-x p1)))
      (is (= 2.5 (point-y p1)))
      (is (point-equal p2 (point-zero)))
      (is (point-equal p3 p1)))))

;;; --- Functions --------------------------------------------------------------

;;;     point-x
;;;     point-y

(test point-x/y
  (with-graphene-point (p)
    (is (= 0.0 (point-x p)))
    (is (= 0.0 (point-y p)))
    (is (= 1.0 (setf (point-x p) 1.0)))
    (is (= 2.0 (setf (point-y p) 2.0)))
    (is (= 1.0 (point-x p)))
    (is (= 2.0 (point-y p)))))

;;;     graphene_point_alloc
;;;     graphene_point_free

(test point-alloc/free
  (let ((point nil))
    (is (pointerp (setf point (point-alloc))))
    (is (= 0.0 (point-x point)))
    (is (= 0.0 (point-y point)))
    (is-false (point-free point))))

;;;     graphene_point_zero

(test point-zero.1
  (let ((p (point-zero)))
    (is (pointerp p))
    (is (= 0.0 (point-x p)))
    (is (= 0.0 (point-y p)))))

(test point-zero.2
  (with-graphene-points (p1 (p2 1 2))
    (is (point-equal p1 (point-zero)))
    (is (not (point-equal p2 (point-zero))))))

;;;     graphene_point_init

(test point-init.1
  (with-graphene-point (p)
    (is (point-equal p (point-zero)))
    (is (pointer-eq p (point-init p 1.0 2.0)))
    (is (= 1.0 (point-x p)))
    (is (= 2.0 (point-y p)))
    (is (pointer-eq p (point-init p 1 1/2)))
    (is (= 1.0 (point-x p)))
    (is (= 0.5 (point-y p)))
    (is (pointer-eq p (point-init p 2.5d0 3.5d0)))
    (is (= 2.5 (point-x p)))
    (is (= 3.5 (point-y p)))))

(test point-init.2
  (with-graphene-point (p 1.0 2.0)
    (is (= 1.0 (point-x p)))
    (is (= 2.0 (point-y p)))))

(test point-init.3
  (with-graphene-points ((p1 1.0 2.0) p2)
    (is (= 1.0 (point-x p1)))
    (is (= 2.0 (point-y p1)))
    (is (= 0.0 (point-x p2)))
    (is (= 0.0 (point-y p2)))))

;;;     graphene_point_init_from_point

(test point-init-from-point
  (with-graphene-points ((p1 1.0 2.0) p2)
    (is (pointer-eq p2 (point-init-from-point p2 p1)))
    (is (= 1.0 (point-x p2)))
    (is (= 2.0 (point-y p2)))))

;;;     graphene_point_init_from_vec2

(test point-init-from-vec2
  (with-graphene-vec2 (vector 1.5 2.5)
    (with-graphene-point (p)
      (is (pointer-eq p (point-init-from-vec2 p vector)))
      (is (= (vec2-x vector) (point-x p)))
      (is (= (vec2-y vector) (point-y p))))))

;;;     graphene_point_to_vec2

(test point-to-vec2
  (with-graphene-point (p 2.5 3.5)
    (with-graphene-vec2 (v)
      (is (pointer-eq v (point-to-vec2 p v)))
      (is (= 2.5 (vec2-x v)))
      (is (= 3.5 (vec2-y v))))))

;;;     graphene_point_equal

(test point-equal
  (with-graphene-points ((p1 1.0 2.0) (p2 1.0 2.0) (p3 0 0))
    (is (point-equal p1 p2))
    (is (not (point-equal p1 p3)))
    (is (not (point-equal p2 p3)))))

;;;     graphene_point_near

(test point-near
  (with-graphene-points ((p1 0.000 0.000) (p2 0.001 0.000) (p3 0.000 0.001))
    (is (point-near p1 p2 0.01))
    (is (point-near p1 p3 0.01))
    (is (not (point-near p1 p2 0.001)))
    (is (not (point-near p1 p3 0.001)))))

;;;     graphene_point_distance

(test point-distance
  (with-graphene-points ((p1 0.0 0.0) (p2 1.0 0.0) (p3 0.0 1.0))
    (is (equal '(1.0 1.0 0.0)
               (multiple-value-list (point-distance p1 p2))))
    (is (equal '(1.0 0.0 1.0)
               (multiple-value-list (point-distance p1 p3))))
    (is (equal '(1.4142135 1.0 1.0)
               (multiple-value-list (point-distance p2 p3))))))

;;;     graphene_point_interpolate

(test point-interpolate
  (with-graphene-points ((p1 0.0 0.0) (p2 1.0 0.0) (p3 0.0 1.0)
                         result)
    (is (pointer-eq result (point-interpolate p1 p2 0.1 result)))
    (is (= 0.1 (point-x result)))
    (is (= 0.0 (point-y result)))
    (is (pointer-eq result (point-interpolate p1 p3 0.1 result)))
    (is (= 0.0 (point-x result)))
    (is (= 0.1 (point-y result)))))

;;; 2022-10-1
