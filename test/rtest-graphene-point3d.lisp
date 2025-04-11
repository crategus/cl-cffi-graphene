(in-package :graphene-test)

(def-suite graphene-point3d :in graphene-suite)
(in-suite graphene-point3d)

;;; --- Types and Values -------------------------------------------------------

;;;     graphene_point3d_t

(test graphene-point3d-t-structure
  (is (= 12 (cffi:foreign-type-size '(:struct graphene:point3d-t))))
  (is (equal '(graphene::x graphene::y graphene::z)
             (cffi:foreign-slot-names '(:struct graphene:point3d-t))))
  (cffi:with-foreign-object (point '(:struct graphene:point3d-t))
    (is (= 0 (setf (graphene:point3d-x point) 0)))
    (is (= 0 (graphene:point3d-x point)))
    (is (= 0 (setf (graphene:point3d-y point) 0)))
    (is (= 0 (graphene:point3d-y point)))
    (is (= 0 (setf (graphene:point3d-z point) 0)))
    (is (= 0 (graphene:point3d-z point)))))

;;; --- Macros -----------------------------------------------------------------

;;;     with-point3d

(test graphene-with-point3d.1
  (graphene:with-point3d (p)
    (is (= 0.0 (graphene:point3d-x p)))
    (is (= 0.0 (graphene:point3d-y p)))
    (is (= 0.0 (graphene:point3d-y p)))))

(test graphene-with-point3d.2
  (graphene:with-point3d (p 1 2 3)
    (is (= 1.0 (graphene:point3d-x p)))
    (is (= 2.0 (graphene:point3d-y p)))
    (is (= 3.0 (graphene:point3d-z p)))))

(test graphene-with-point3d.3
  (graphene:with-point3ds ((p1 3/2 4/2 5/2) (p p1))
    (is (= 1.5 (graphene:point3d-x p)))
    (is (= 2.0 (graphene:point3d-y p)))
    (is (= 2.5 (graphene:point3d-z p)))))

(test graphene-with-point3d.4
  (graphene:with-point3ds ((p1 5 6 7) (p (p1 graphene:point3d-t)))
    (is (= 5.0 (graphene:point3d-x p)))
    (is (= 6.0 (graphene:point3d-y p)))
    (is (= 7.0 (graphene:point3d-z p)))))

(test graphene-with-point3d.5
  (graphene:with-vec3 (v 1.5 2.5 3.5)
    (graphene:with-point3d (p (v graphene:vec3-t))
      (is (= 1.5 (graphene:point3d-x p)))
      (is (= 2.5 (graphene:point3d-y p)))
      (is (= 3.5 (graphene:point3d-z p))))))

(test graphene-with-point3d.6
  (let ((a 3.5) (b 4.5) (c 5.5))
    (graphene:with-point3d (p a b c)
      (is (= 3.5 (graphene:point3d-x p)))
      (is (= 4.5 (graphene:point3d-y p)))
      (is (= 5.5 (graphene:point3d-z p))))))

(test graphene-with-point3d.7
  (graphene:with-point3d (p (graphene:point3d-zero))
    (is (= 0.0 (graphene:point3d-x p)))
    (is (= 0.0 (graphene:point3d-y p)))
    (is (= 0.0 (graphene:point3d-z p)))))

(test graphene-with-point3d.8
  (graphene:with-point3d (p ((graphene:point3d-zero) graphene:point3d-t))
    (is (= 0.0 (graphene:point3d-x p)))
    (is (= 0.0 (graphene:point3d-y p)))
    (is (= 0.0 (graphene:point3d-z p)))))

(test graphene-with-point3d.9
  (graphene:with-point3d (p ((graphene:vec3-one) graphene:vec3-t))
    (is (= 1.0 (graphene:point3d-x p)))
    (is (= 1.0 (graphene:point3d-y p)))
    (is (= 1.0 (graphene:point3d-z p)))))

;;;     with-point3ds

(test graphene-with-point3ds.1
  (graphene:with-point3ds ((p1) (p2) (p3))
    (is (graphene:point3d-equal p1 (graphene:point3d-zero)))
    (is (graphene:point3d-equal p2 (graphene:point3d-zero)))
    (is (graphene:point3d-equal p3 (graphene:point3d-zero)))))

(test graphene-with-point3ds.2
  (graphene:with-point3ds (p1 p2 p3)
    (is (graphene:point3d-equal p1 (graphene:point3d-zero)))
    (is (graphene:point3d-equal p2 (graphene:point3d-zero)))
    (is (graphene:point3d-equal p3 (graphene:point3d-zero)))))

(test graphene-with-point3ds.3
  (graphene:with-point3ds (p1 (p2 1 2 3) (p3 p2) (p4 (p3 graphene:point3d-t)))
    (is (graphene:point3d-equal p1 (graphene:point3d-zero)))
    (is (= 1.0 (graphene:point3d-x p2)))
    (is (= 2.0 (graphene:point3d-y p2)))
    (is (= 3.0 (graphene:point3d-z p2)))
    (is (graphene:point3d-equal p3 p2))
    (is (graphene:point3d-equal p4 p3))))

(test graphene-with-point3ds.4
  (graphene:with-vec3 (v 1.5 2.5 3.5)
    (graphene:with-point3ds ((p1 (v graphene:vec3-t)) p2 (p3 p1))
      (is (= 1.5 (graphene:point3d-x p1)))
      (is (= 2.5 (graphene:point3d-y p1)))
      (is (= 3.5 (graphene:point3d-z p1)))
      (is (graphene:point3d-equal p2 (graphene:point3d-zero)))
      (is (graphene:point3d-equal p3 p1)))))

;;; --- Functions --------------------------------------------------------------

;;;     graphene:point3d-x
;;;     graphene:point3d-y
;;;     graphene:point3d-z

(test graphene-point3d-x/y/z
  (graphene:with-point3d (p 1 2 3)
    (is (= 1.0 (graphene:point3d-x p)))
    (is (= 2.0 (graphene:point3d-y p)))
    (is (= 3.0 (graphene:point3d-z p)))
    (is (= 1.5 (setf (graphene:point3d-x p) (/ 3 2))))
    (is (= 2.5 (setf (graphene:point3d-y p) (/ 5 2))))
    (is (= 3.5 (setf (graphene:point3d-z p) (/ 7 2))))
    (is (= 1.5 (graphene:point3d-x p)))
    (is (= 2.5 (graphene:point3d-y p)))
    (is (= 3.5 (graphene:point3d-z p)))))

;;;     graphene_point3d_alloc
;;;     graphene_point3d_free

(test graphene-point3d-alloc/free
  (let (point)
    (is (cffi:pointerp (setf point (graphene:point3d-alloc))))
    (is (= 0.0 (graphene:point3d-x point)))
    (is (= 0.0 (graphene:point3d-y point)))
    (is (= 0.0 (graphene:point3d-z point)))
    (is-false (graphene:point-free point))))

;;;     graphene_point3d_zero

(test graphene-point3d-zero.1
  (is (= 0.0 (graphene:point3d-x (graphene:point3d-zero))))
  (is (= 0.0 (graphene:point3d-y (graphene:point3d-zero))))
  (is (= 0.0 (graphene:point3d-z (graphene:point3d-zero)))))

(test graphene-point3d-zero.2
  (graphene:with-point3ds (p1 (p2 1 2 3))
    (is (graphene:point3d-equal p1 (graphene:point3d-zero)))
    (is (not (graphene:point3d-equal p2 (graphene:point3d-zero))))))

;;;     graphene_point3d_init

(test graphene-point3d-init
  (graphene:with-point3d (p)
    (is (graphene:point3d-equal p (graphene:point3d-zero)))
    (is (cffi:pointer-eq p (graphene:point3d-init p 1 2 3)))
    (is (= 1.0 (graphene:point3d-x p)))
    (is (= 2.0 (graphene:point3d-y p)))
    (is (= 3.0 (graphene:point3d-z p)))
    (is (cffi:pointer-eq p (graphene:point3d-init p 1/2 2.0d0 3.0)))
    (is (= 0.5 (graphene:point3d-x p)))
    (is (= 2.0 (graphene:point3d-y p)))
    (is (= 3.0 (graphene:point3d-z p)))))

;;;     graphene_point3d_init_from_point

(test graphene-point3d-init-from-point
  (graphene:with-point3ds (p (p1 1 2 3))
    (is (cffi:pointer-eq p (graphene:point3d-init-from-point p p1)))
    (is (= 1.0 (graphene:point3d-x p)))
    (is (= 2.0 (graphene:point3d-y p)))
    (is (= 3.0 (graphene:point3d-z p)))))

;;;     graphene_point3d_init_from_vec3

(test graphene-point3d-init-from-vec3
  (graphene:with-vec3 (v 1 2 3)
    (graphene:with-point3d (p)
      (is (cffi:pointer-eq p (graphene:point3d-init-from-vec3 p v)))
      (is (= 1.0 (graphene:point3d-x p)))
      (is (= 2.0 (graphene:point3d-y p)))
      (is (= 3.0 (graphene:point3d-z p))))))

;;;     graphene_point3d_to_vec3

(test graphene-point3d-to-vec3
  (graphene:with-vec3 (v)
    (graphene:with-point3d (p 1 2 3)
      (is (cffi:pointer-eq v (graphene:point3d-to-vec3 p v)))
      (is (equal '(1.0 2.0 3.0) (graphene:vec3-to-float v))))))

;;;     graphene_point3d_equal

(test graphene-point3d-equal
  (graphene:with-point3ds ((p1 1 2 3) (p2 1 2 3) (p3 4 5 6))
    (is (graphene:point3d-equal p1 p2))
    (is (not (graphene:point3d-equal p1 p3)))
    (is (not (graphene:point3d-equal p2 p3)))))

;;;     graphene_point3d_near

(test graphene-point3d-near
  (graphene:with-point3ds ((p1 0.000 0.000 0.000)
                           (p2 0.001 0.000 0.000)
                           (p3 0.000 0.001 0.000))
    (is (graphene:point3d-near p1 p2 0.01))
    (is (graphene:point3d-near p1 p3 0.01))
    (is (not (graphene:point3d-near p1 p2 0.001)))
    (is (not (graphene:point3d-near p1 p3 0.001)))))

;;;     graphene_point3d_distance

(test graphene-point3d-distance
  (graphene:with-point3ds ((p1 1 0 0) (p2 0 1 0) (p3 0 0 1) delta)
    (multiple-value-bind (dist delta)
        (graphene:point3d-distance p1 p2 delta)
      (is (= (sqrt 2.0) dist))
      (is (= 1.0 (graphene:point3d-x delta)))
      (is (= 1.0 (graphene:point3d-y delta)))
      (is (= 0.0 (graphene:point3d-z delta))))
    (multiple-value-bind (dist delta)
        (graphene:point3d-distance p1 p3 delta)
      (is (= (sqrt 2.0) dist))
      (is (= 1.0 (graphene:point3d-x delta)))
      (is (= 0.0 (graphene:point3d-y delta)))
      (is (= 1.0 (graphene:point3d-z delta))))
    (multiple-value-bind (dist delta)
        (graphene:point3d-distance p2 p3 delta)
      (is (= (sqrt 2.0) dist))
      (is (= 0.0 (graphene:point3d-x delta)))
      (is (= 1.0 (graphene:point3d-y delta)))
      (is (= 1.0 (graphene:point3d-z delta))))))

;;;     graphene_point3d_interpolate

(test graphene-point3d-interpolate
  (graphene:with-point3ds ((p1 0.0 0.0 0.0)
                           (p2 1.0 0.0 0.0)
                           (p3 0.0 1.0 0.0)
                           result)
    (is (cffi:pointer-eq result (graphene:point3d-interpolate p1 p2 0.1 result)))
    (is (= 0.1 (graphene:point3d-x result)))
    (is (= 0.0 (graphene:point3d-y result)))
    (is (= 0.0 (graphene:point3d-z result)))
    (is (cffi:pointer-eq result (graphene:point3d-interpolate p1 p3 0.1 result)))
    (is (= 0.0 (graphene:point3d-x result)))
    (is (= 0.1 (graphene:point3d-y result)))
    (is (= 0.0 (graphene:point3d-z result)))
    (is (cffi:pointer-eq result (graphene:point3d-interpolate p2 p3 0.1 result)))
    (is (= 0.9 (graphene:point3d-x result)))
    (is (= 0.1 (graphene:point3d-y result)))
    (is (= 0.0 (graphene:point3d-z result)))))

;;;     graphene_point3d_scale

(test graphene-point3d-scale.1
  (graphene:with-point3ds ((p 1 2 3) result)
    (is (cffi:pointer-eq result (graphene:point3d-scale p 2 result)))
    (is (= 2.0 (graphene:point3d-x result)))
    (is (= 4.0 (graphene:point3d-y result)))
    (is (= 6.0 (graphene:point3d-z result)))))

(test graphene-point3d-scale.2
  (graphene:with-point3ds ((p 1 2 3) result)
    (is (cffi:pointer-eq result (graphene:point3d-scale p 1/2 result)))
    (is (= 0.5 (graphene:point3d-x result)))
    (is (= 1.0 (graphene:point3d-y result)))
    (is (= 1.5 (graphene:point3d-z result)))))

;;;     graphene_point3d_cross

(test graphene-point3d-cross
  (graphene:with-point3ds ((p1 1 0 0) (p2 0 1 0) (p3 0 0 1) result)
    (is (cffi:pointer-eq result (graphene:point3d-cross p1 p2 result)))
    (is (=  0.0 (graphene:point3d-x result)))
    (is (=  0.0 (graphene:point3d-y result)))
    (is (=  1.0 (graphene:point3d-z result)))
    (is (cffi:pointer-eq result (graphene:point3d-cross p1 p3 result)))
    (is (=  0.0 (graphene:point3d-x result)))
    (is (= -1.0 (graphene:point3d-y result)))
    (is (=  0.0 (graphene:point3d-z result)))
    (is (cffi:pointer-eq result (graphene:point3d-cross p2 p3 result)))
    (is (=  1.0 (graphene:point3d-x result)))
    (is (=  0.0 (graphene:point3d-y result)))
    (is (=  0.0 (graphene:point3d-z result)))))

;;;     graphene_point3d_dot

(test graphene-point3d-dot
  (graphene:with-point3ds ((p1 1 1 1) (p2 2 2 2))
    (is (=  3.0 (graphene:point3d-dot p1 p1)))
    (is (= 12.0 (graphene:point3d-dot p2 p2)))
    (is (=  6.0 (graphene:point3d-dot p1 p2)))))

;;;     graphene_point3d_length

(test graphene-point3d-length
  (graphene:with-point3ds ((p1 1 1 1) (p2 2 2 2))
    (is (= (sqrt  3.0) (graphene:point3d-length p1)))
    (is (= (sqrt 12.0) (graphene:point3d-length p2)))))

;;;     graphene_point3d_normalize

(test graphene-point3d-normalize
  (graphene:with-point3ds ((p1 4 0 0) (p2 0 5 0) (p3 0 0 6) result)
    (is (cffi:pointer-eq result (graphene:point3d-normalize p1 result)))
    (is (approx-equal 1.0 (graphene:point3d-length result)))
    (is (cffi:pointer-eq result (graphene:point3d-normalize p2 result)))
    (is (approx-equal 1.0 (graphene:point3d-length result)))
    (is (cffi:pointer-eq result (graphene:point3d-normalize p3 result)))
    (is (approx-equal 1.0 (graphene:point3d-length result)))))

;;;     graphene_point3d_normalize_viewport

(test graphene-point3d-normalize-viewport
  (graphene:with-point3ds ((p 1 1 0) result)
    (graphene:with-rect (viewport 0 0 4 4)
        (is (cffi:pointer-eq result
                (graphene:point3d-normalize-viewport p viewport 0 1 result)))
        (is (= -0.5 (graphene:point3d-x result)))
        (is (= -0.5 (graphene:point3d-y result)))
        (is (= -1.0 (graphene:point3d-z result))))))

;;; 2025-4-2
