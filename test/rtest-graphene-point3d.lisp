(in-package :graphene-test)

(def-suite graphene-point3d :in graphene-suite)
(in-suite graphene-point3d)

;;; --- Types and Values -------------------------------------------------------

;;;     graphene_point3d_t

;;; --- Macros -----------------------------------------------------------------

;;;     with-graphene-point3d

(test with-graphene-point3d.1
  (with-graphene-point3d (p)
    (is (= 0.0 (point3d-x p)))
    (is (= 0.0 (point3d-y p)))
    (is (= 0.0 (point3d-y p)))))

(test with-graphene-point3d.2
  (with-graphene-point3d (p 1 2 3)
    (is (= 1.0 (point3d-x p)))
    (is (= 2.0 (point3d-y p)))
    (is (= 3.0 (point3d-z p)))))

(test with-graphene-point3d.3
  (with-graphene-point3ds ((p1 3/2 4/2 5/2) (p p1))
    (is (= 1.5 (point3d-x p)))
    (is (= 2.0 (point3d-y p)))
    (is (= 2.5 (point3d-z p)))))

(test with-graphene-point3d.4
  (with-graphene-point3ds ((p1 5 6 7) (p (p1 point3d-t)))
    (is (= 5.0 (point3d-x p)))
    (is (= 6.0 (point3d-y p)))
    (is (= 7.0 (point3d-z p)))))

(test with-graphene-point3d.5
  (with-graphene-vec3 (v 1.5 2.5 3.5)
    (with-graphene-point3d (p (v vec3-t))
      (is (= 1.5 (point3d-x p)))
      (is (= 2.5 (point3d-y p)))
      (is (= 3.5 (point3d-z p))))))

(test with-graphene-point.6
  (let ((a 3.5) (b 4.5) (c 5.5))
    (with-graphene-point3d (p a b c)
      (is (= 3.5 (point3d-x p)))
      (is (= 4.5 (point3d-y p)))
      (is (= 5.5 (point3d-z p))))))

;;;     with-graphene-point3ds

(test with-graphene-point3ds.1
  (with-graphene-point3ds ((p1) (p2) (p3))
    (is (point3d-equal p1 (point3d-zero)))
    (is (point3d-equal p2 (point3d-zero)))
    (is (point3d-equal p3 (point3d-zero)))))

(test with-graphene-point3ds.2
  (with-graphene-point3ds (p1 p2 p3)
    (is (point3d-equal p1 (point3d-zero)))
    (is (point3d-equal p2 (point3d-zero)))
    (is (point3d-equal p3 (point3d-zero)))))

(test with-graphene-point3ds.3
  (with-graphene-point3ds (p1 (p2 1 2 3) (p3 p2) (p4 (p3 point3d-t)))
    (is (point3d-equal p1 (point3d-zero)))
    (is (= 1.0 (point3d-x p2)))
    (is (= 2.0 (point3d-y p2)))
    (is (= 3.0 (point3d-z p2)))
    (is (point3d-equal p3 p2))
    (is (point3d-equal p4 p3))))

(test with-graphene-point3ds.4
  (with-graphene-vec3 (v 1.5 2.5 3.5)
    (with-graphene-point3ds ((p1 (v vec3-t)) p2 (p3 p1))
      (is (= 1.5 (point3d-x p1)))
      (is (= 2.5 (point3d-y p1)))
      (is (= 3.5 (point3d-z p1)))
      (is (point3d-equal p2 (point3d-zero)))
      (is (point3d-equal p3 p1)))))

;;; --- Functions --------------------------------------------------------------

;;;     point3d-x
;;;     point3d-y
;;;     point3d-z

(test point3d-x/y/z
  (with-graphene-point3d (p 1 2 3)
    (is (= 1.0 (point3d-x p)))
    (is (= 2.0 (point3d-y p)))
    (is (= 3.0 (point3d-z p)))
    (is (= 1.5 (setf (point3d-x p) 1.5)))
    (is (= 2.5 (setf (point3d-y p) 2.5)))
    (is (= 3.5 (setf (point3d-z p) 3.5)))
    (is (= 1.5 (point3d-x p)))
    (is (= 2.5 (point3d-y p)))
    (is (= 3.5 (point3d-z p)))))

;;;     graphene_point3d_alloc
;;;     graphene_point3d_free

(test point3d-alloc/free
  (let ((point nil))
    (is (pointerp (setf point (point3d-alloc))))
    (is (= 0.0 (point3d-x point)))
    (is (= 0.0 (point3d-y point)))
    (is (= 0.0 (point3d-z point)))
    (is-false (point-free point))))

;;;     graphene_point3d_zero

(test point3d-zero.1
  (is (= 0.0 (point3d-x (point3d-zero))))
  (is (= 0.0 (point3d-y (point3d-zero))))
  (is (= 0.0 (point3d-z (point3d-zero)))))

(test point3d-zero.2
  (with-graphene-point3ds (p1 (p2 1 2 3))
    (is (point3d-equal p1 (point3d-zero)))
    (is (not (point3d-equal p2 (point3d-zero))))))

;;;     graphene_point3d_init

(test point3d-init
  (with-graphene-point3d (p)
    (is (point3d-equal p (point3d-zero)))
    (is (pointer-eq p (point3d-init p 1 2 3)))
    (is (= 1.0 (point3d-x p)))
    (is (= 2.0 (point3d-y p)))
    (is (= 3.0 (point3d-z p)))
    (is (pointer-eq p (point3d-init p 1/2 2.0d0 3.0)))
    (is (= 0.5 (point3d-x p)))
    (is (= 2.0 (point3d-y p)))
    (is (= 3.0 (point3d-z p)))))

;;;     graphene_point3d_init_from_point

(test point3d-init-from-point
  (with-graphene-point3ds (p (p1 1 2 3))
    (is (pointer-eq p (point3d-init-from-point p p1)))
    (is (= 1.0 (point3d-x p)))
    (is (= 2.0 (point3d-y p)))
    (is (= 3.0 (point3d-z p)))))

;;;     graphene_point3d_init_from_vec3

(test point3d-init-from-vec3
  (with-graphene-vec3 (v 1 2 3)
    (with-graphene-point3d (p)
      (is (pointer-eq p (point3d-init-from-vec3 p v)))
      (is (= 1.0 (point3d-x p)))
      (is (= 2.0 (point3d-y p)))
      (is (= 3.0 (point3d-z p))))))

;;;     graphene_point3d_to_vec3

(test point3d-to-vec3
  (with-graphene-vec3 (v)
    (with-graphene-point3d (p 1 2 3)
      (is (pointer-eq v (point3d-to-vec3 p v)))
      (is (equal '(1.0 2.0 3.0) (vec3-to-float v))))))

;;;     graphene_point3d_equal

(test point3d-equal
  (with-graphene-point3ds ((p1 1 2 3) (p2 1 2 3) (p3 4 5 6))
    (is (point3d-equal p1 p2))
    (is (not (point3d-equal p1 p3)))
    (is (not (point3d-equal p2 p3)))))

;;;     graphene_point3d_near

(test graphene-point3d-near
  (with-graphene-point3ds ((p1 0.000 0.000 0.000)
                           (p2 0.001 0.000 0.000)
                           (p3 0.000 0.001 0.000))
    (is (point3d-near p1 p2 0.01))
    (is (point3d-near p1 p3 0.01))
    (is (not (point3d-near p1 p2 0.001)))
    (is (not (point3d-near p1 p3 0.001)))))

;;;     graphene_point3d_distance

(test point3d-distance
  (with-graphene-point3ds ((p1 1 0 0) (p2 0 1 0) (p3 0 0 1) delta)
    (multiple-value-bind (dist delta)
        (point3d-distance p1 p2 delta)
      (is (= (sqrt 2.0) dist))
      (is (= 1.0 (point3d-x delta)))
      (is (= 1.0 (point3d-y delta)))
      (is (= 0.0 (point3d-z delta))))
    (multiple-value-bind (dist delta)
        (point3d-distance p1 p3 delta)
      (is (= (sqrt 2.0) dist))
      (is (= 1.0 (point3d-x delta)))
      (is (= 0.0 (point3d-y delta)))
      (is (= 1.0 (point3d-z delta))))
    (multiple-value-bind (dist delta)
        (point3d-distance p2 p3 delta)
      (is (= (sqrt 2.0) dist))
      (is (= 0.0 (point3d-x delta)))
      (is (= 1.0 (point3d-y delta)))
      (is (= 1.0 (point3d-z delta))))))

;;;     graphene_point3d_interpolate

(test graphene-point3d-interpolate
  (with-graphene-point3ds ((p1 0.0 0.0 0.0)
                           (p2 1.0 0.0 0.0)
                           (p3 0.0 1.0 0.0)
                           result)
    (is (pointer-eq result (point3d-interpolate p1 p2 0.1 result)))
    (is (= 0.1 (point3d-x result)))
    (is (= 0.0 (point3d-y result)))
    (is (= 0.0 (point3d-z result)))
    (is (pointer-eq result (point3d-interpolate p1 p3 0.1 result)))
    (is (= 0.0 (point3d-x result)))
    (is (= 0.1 (point3d-y result)))
    (is (= 0.0 (point3d-z result)))
    (is (pointer-eq result (point3d-interpolate p2 p3 0.1 result)))
    (is (= 0.9 (point3d-x result)))
    (is (= 0.1 (point3d-y result)))
    (is (= 0.0 (point3d-z result)))))

;;;     graphene_point3d_scale

(test point3d-scale.1
  (with-graphene-point3ds ((p 1 2 3) result)
    (is (pointer-eq result (point3d-scale p 2 result)))
    (is (= 2.0 (point3d-x result)))
    (is (= 4.0 (point3d-y result)))
    (is (= 6.0 (point3d-z result)))))

(test point3d-scale.2
  (with-graphene-point3ds ((p 1 2 3) result)
    (is (pointer-eq result (point3d-scale p 1/2 result)))
    (is (= 0.5 (point3d-x result)))
    (is (= 1.0 (point3d-y result)))
    (is (= 1.5 (point3d-z result)))))

;;;     graphene_point3d_cross

(test point3d-cross
  (with-graphene-point3ds ((p1 1 0 0) (p2 0 1 0) (p3 0 0 1) result)
    (is (pointer-eq result (point3d-cross p1 p2 result)))
    (is (=  0.0 (point3d-x result)))
    (is (=  0.0 (point3d-y result)))
    (is (=  1.0 (point3d-z result)))
    (is (pointer-eq result (point3d-cross p1 p3 result)))
    (is (=  0.0 (point3d-x result)))
    (is (= -1.0 (point3d-y result)))
    (is (=  0.0 (point3d-z result)))
    (is (pointer-eq result (point3d-cross p2 p3 result)))
    (is (=  1.0 (point3d-x result)))
    (is (=  0.0 (point3d-y result)))
    (is (=  0.0 (point3d-z result)))))

;;;     graphene_point3d_dot

(test point3d-dot
  (with-graphene-point3ds ((p1 1 1 1) (p2 2 2 2))
    (is (=  3.0 (point3d-dot p1 p1)))
    (is (= 12.0 (point3d-dot p2 p2)))
    (is (=  6.0 (point3d-dot p1 p2)))))

;;;     graphene_point3d_length

(test point3d-length
  (with-graphene-point3ds ((p1 1 1 1) (p2 2 2 2))
    (is (= (sqrt  3.0) (point3d-length p1)))
    (is (= (sqrt 12.0) (point3d-length p2)))))

;;;     graphene_point3d_normalize

(test point3d-normalize
  (with-graphene-point3ds ((p1 4 0 0) (p2 0 5 0) (p3 0 0 6) result)
    (is (pointer-eq result (point3d-normalize p1 result)))
    (is (approx-equal 1.0 (point3d-length result)))
    (is (pointer-eq result (point3d-normalize p2 result)))
    (is (approx-equal 1.0 (point3d-length result)))
    (is (pointer-eq result (point3d-normalize p3 result)))
    (is (approx-equal 1.0 (point3d-length result)))))

;;;     graphene_point3d_normalize_viewport

(test point3d-normalize-viewport
  (with-graphene-point3ds ((p 1 1 0) result)
    (with-graphene-rect (viewport 0 0 4 4)
        (is (pointer-eq result
                        (point3d-normalize-viewport p viewport 0 1 result)))
        (is (= -0.5 (point3d-x result)))
        (is (= -0.5 (point3d-y result)))
        (is (= -1.0 (point3d-z result))))))

;;; 2022-10-1
