(in-package :graphene-test)

(def-suite graphene-point :in graphene-suite)
(in-suite graphene-point)

;;; --- Types and Values -------------------------------------------------------

;;;     graphene_point_t

(test graphene-point-t-structure
  (is (= 8 (cffi:foreign-type-size '(:struct graphene:point-t))))
  (is (equal '(graphene::x graphene::y)
             (cffi:foreign-slot-names '(:struct graphene:point-t))))
  (cffi:with-foreign-object (point '(:struct graphene:point-t))
    (is (= 0 (setf (graphene:point-x point) 0)))
    (is (= 0 (graphene:point-x point)))
    (is (= 0 (setf (graphene:point-y point) 0)))
    (is (= 0 (graphene:point-y point)))))

;;; --- Macros -----------------------------------------------------------------

;;;     with-point

(test graphene-with-point.1
  (graphene:with-point (p)
    (is (= 0.0 (graphene:point-x p)))
    (is (= 0.0 (graphene:point-y p)))))

(test graphene-with-point.2
  (graphene:with-point (p 1 2)
    (is (= 1.0 (graphene:point-x p)))
    (is (= 2.0 (graphene:point-y p)))))

(test graphene-with-point.3
  (graphene:with-points ((p1 3 4) (p p1))
    (is (= 3.0 (graphene:point-x p)))
    (is (= 4.0 (graphene:point-y p)))))

(test graphene-with-point.4
  (graphene:with-points ((p1 5 6) (p (p1 graphene:point-t)))
    (is (= 5.0 (graphene:point-x p)))
    (is (= 6.0 (graphene:point-y p)))))

(test graphene-with-point.5
  (graphene:with-vec2 (v 1.5 2.5)
    (graphene:with-point (p (v graphene:vec2-t))
      (is (= 1.5 (graphene:point-x p)))
      (is (= 2.5 (graphene:point-y p))))))

(test graphene-with-point.6
  (let ((a 3.5) (b 4.5))
    (graphene:with-point (p a b)
      (is (= 3.5 (graphene:point-x p)))
      (is (= 4.5 (graphene:point-y p))))))

;;;     with-points

(test graphene-with-points.1
  (graphene:with-points ((p1) (p2) (p3))
    (is (graphene:point-equal p1 (graphene:point-zero)))
    (is (graphene:point-equal p2 (graphene:point-zero)))
    (is (graphene:point-equal p3 (graphene:point-zero)))))

(test graphene-with-points.2
  (graphene:with-points (p1 p2 p3)
    (is (graphene:point-equal p1 (graphene:point-zero)))
    (is (graphene:point-equal p2 (graphene:point-zero)))
    (is (graphene:point-equal p3 (graphene:point-zero)))))

(test graphene-with-points.3
  (graphene:with-points (p1 (p2 1 2) (p3 p2) (p4 (p3 graphene:point-t)))
    (is (graphene:point-equal p1 (graphene:point-zero)))
    (is (= 1.0 (graphene:point-x p2)))
    (is (= 2.0 (graphene:point-y p2)))
    (is (graphene:point-equal p3 p2))
    (is (graphene:point-equal p4 p3))))

(test graphene-with-points.4
  (graphene:with-vec2 (v 1.5 2.5)
    (graphene:with-points ((p1 (v graphene:vec2-t)) p2 (p3 p1))
      (is (= 1.5 (graphene:point-x p1)))
      (is (= 2.5 (graphene:point-y p1)))
      (is (graphene:point-equal p2 (graphene:point-zero)))
      (is (graphene:point-equal p3 p1)))))

;;; --- Functions --------------------------------------------------------------

;;;     graphene_point_alloc
;;;     graphene_point_free

(test graphene-point-alloc/free
  (let ((point nil))
    (is (cffi:pointerp (setf point (graphene:point-alloc))))
    (is (= 0.0 (graphene:point-x point)))
    (is (= 0.0 (graphene:point-y point)))
    (is-false (graphene:point-free point))))

;;;     graphene_point_zero

(test graphene-point-zero.1
  (let ((p (graphene:point-zero)))
    (is (cffi:pointerp p))
    (is (= 0.0 (graphene:point-x p)))
    (is (= 0.0 (graphene:point-y p)))))

(test graphene-point-zero.2
  (graphene:with-points (p1 (p2 1 2))
    (is (graphene:point-equal p1 (graphene:point-zero)))
    (is (not (graphene:point-equal p2 (graphene:point-zero))))))

;;;     graphene_point_init

(test graphene-point-init.1
  (graphene:with-point (p)
    (is (graphene:point-equal p (graphene:point-zero)))
    (is (cffi:pointer-eq p (graphene:point-init p 1.0 2.0)))
    (is (= 1.0 (graphene:point-x p)))
    (is (= 2.0 (graphene:point-y p)))
    (is (cffi:pointer-eq p (graphene:point-init p 1 1/2)))
    (is (= 1.0 (graphene:point-x p)))
    (is (= 0.5 (graphene:point-y p)))
    (is (cffi:pointer-eq p (graphene:point-init p 2.5d0 3.5d0)))
    (is (= 2.5 (graphene:point-x p)))
    (is (= 3.5 (graphene:point-y p)))))

(test graphene-point-init.2
  (graphene:with-point (p 1.0 2.0)
    (is (= 1.0 (graphene:point-x p)))
    (is (= 2.0 (graphene:point-y p)))))

(test graphene-point-init.3
  (graphene:with-points ((p1 1.0 2.0) p2)
    (is (= 1.0 (graphene:point-x p1)))
    (is (= 2.0 (graphene:point-y p1)))
    (is (= 0.0 (graphene:point-x p2)))
    (is (= 0.0 (graphene:point-y p2)))))

;;;     graphene_point_init_from_point

(test graphene-point-init-from-point
  (graphene:with-points ((p1 1.0 2.0) p2)
    (is (cffi:pointer-eq p2 (graphene:point-init-from-point p2 p1)))
    (is (= 1.0 (graphene:point-x p2)))
    (is (= 2.0 (graphene:point-y p2)))))

;;;     graphene_point_init_from_vec2

(test graphene-point-init-from-vec2
  (graphene:with-vec2 (vect 1.5 2.5)
    (graphene:with-point (p)
      (is (cffi:pointer-eq p (graphene:point-init-from-vec2 p vect)))
      (is (= (graphene:vec2-x vect) (graphene:point-x p)))
      (is (= (graphene:vec2-y vect) (graphene:point-y p))))))

;;;     graphene_point_to_vec2

(test graphene-point-to-vec2
  (graphene:with-point (p 2.5 3.5)
    (graphene:with-vec2 (v)
      (is (cffi:pointer-eq v (graphene:point-to-vec2 p v)))
      (is (= 2.5 (graphene:vec2-x v)))
      (is (= 3.5 (graphene:vec2-y v))))))

;;;     graphene_point_equal

(test graphene-point-equal
  (graphene:with-points ((p1 1.0 2.0) (p2 1.0 2.0) (p3 0 0))
    (is (graphene:point-equal p1 p2))
    (is (not (graphene:point-equal p1 p3)))
    (is (not (graphene:point-equal p2 p3)))))

;;;     graphene_point_near

(test graphene-point-near
  (graphene:with-points ((p1 0.000 0.000)
                         (p2 0.001 0.000) (p3 0.000 0.001))
    (is (graphene:point-near p1 p2 0.01))
    (is (graphene:point-near p1 p3 0.01))
    (is (not (graphene:point-near p1 p2 0.001)))
    (is (not (graphene:point-near p1 p3 0.001)))))

;;;     graphene_point_distance

(test graphene-point-distance
  (graphene:with-points ((p1 0.0 0.0) (p2 1.0 0.0) (p3 0.0 1.0))
    (is (equal '(1.0 1.0 0.0)
               (multiple-value-list (graphene:point-distance p1 p2))))
    (is (equal '(1.0 0.0 1.0)
               (multiple-value-list (graphene:point-distance p1 p3))))
    (is (equal '(1.4142135 1.0 1.0)
               (multiple-value-list (graphene:point-distance p2 p3))))))

;;;     graphene_point_interpolate

(test graphene-point-interpolate
  (graphene:with-points ((p1 0.0 0.0) (p2 1.0 0.0) (p3 0.0 1.0)
                         result)
    (is (cffi:pointer-eq result (graphene:point-interpolate p1 p2 0.1 result)))
    (is (= 0.1 (graphene:point-x result)))
    (is (= 0.0 (graphene:point-y result)))
    (is (cffi:pointer-eq result (graphene:point-interpolate p1 p3 0.1 result)))
    (is (= 0.0 (graphene:point-x result)))
    (is (= 0.1 (graphene:point-y result)))))

;;; 2023-12-10
