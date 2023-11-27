(in-package :graphene-test)

(def-suite graphene-quad :in graphene-suite)
(in-suite graphene-quad)

;;; --- Types and Values -------------------------------------------------------

;;;     graphene_quad_t

;;; --- Macros -----------------------------------------------------------------

;; no argument
(test with-graphene-quad.1
  (graphene:with-graphene-quad (q)
    (is (cffi:pointerp q))))

;; four points
(test with-graphene-quad.2
  (graphene:with-graphene-points (p1 p2 p3 p4)
    (graphene:with-graphene-quad (q p1 p2 p3 p4)
      (is (cffi:pointerp q)))))

;; a rectangle
(test with-graphene-quad.3
  (graphene:with-graphene-rect (r 1 2 3 4)
    (graphene:with-graphene-quad (q r)
      (is (cffi:pointerp q)))))

;;; --- Functions --------------------------------------------------------------

;;;     graphene_quad_alloc
;;;     graphene_quad_free

(test graphene-quad-alloc
  (let ((quad nil))
    (is (cffi:pointerp (setf quad (graphene:quad-alloc))))
    (is-false (graphene:quad-free quad))))

;;;     graphene_quad_init
;;;     graphene_quad_init_from_rect

;;;     graphene_quad_init_from_points

(test graphene-quad-init-from-points
  (graphene:with-graphene-points (p0 p1 p2 p3)
    (graphene:with-graphene-quad (q)
      (is (cffi:pointer-eq 
              q 
              (graphene:quad-init-from-points q 
                                              (list p0 p1 p2 p3)))))))

;;;     graphene_quad_contains

(test graphene-quad-contains
  (graphene:with-graphene-points ((p0 1 2) (p1 4 2) (p2 4 6) (p3 1 6) 
                                  (pt1 3 4) (pt2 3 10))
    (graphene:with-graphene-quad (q p0 p1 p2 p3)
      (is-true (graphene:quad-contains q pt1))
      (is-false (graphene:quad-contains q pt2)))))

;;;     graphene_quad_bounds

(test graphene-quad-bounds
  (graphene:with-graphene-points ((p0 1 2) (p1 4 2) (p2 4 6) (p3 1 6))
    (graphene:with-graphene-rect (result)
      (graphene:with-graphene-quad (q p0 p1 p2 p3)
        (is (cffi:pointer-eq result (graphene:quad-bounds q result)))
        (is (= 1.0 (graphene:rect-x result)))
        (is (= 2.0 (graphene:rect-y result)))
        (is (= 3.0 (graphene:rect-width result)))
        (is (= 4.0 (graphene:rect-height result)))))))

;;;     graphene_quad_get_point

(test graphene-quad-point
  (graphene:with-graphene-point (result)
    (graphene:with-graphene-rect (r 1 2 3 4)
      (graphene:with-graphene-quad (q r)
        (is (cffi:pointer-eq result (graphene:quad-point q 0 result)))
        (is (= 1.0 (graphene:point-x result)))
        (is (= 2.0 (graphene:point-y result)))
        (is (cffi:pointer-eq result (graphene:quad-point q 1 result)))
        (is (= 4.0 (graphene:point-x result)))
        (is (= 2.0 (graphene:point-y result)))
        (is (cffi:pointer-eq result (graphene:quad-point q 2 result)))
        (is (= 4.0 (graphene:point-x result)))
        (is (= 6.0 (graphene:point-y result)))
        (is (cffi:pointer-eq result (graphene:quad-point q 3 result)))
        (is (= 1.0 (graphene:point-x result)))
        (is (= 6.0 (graphene:point-y result)))))))

;;; --- 2023-11-20 -------------------------------------------------------------
