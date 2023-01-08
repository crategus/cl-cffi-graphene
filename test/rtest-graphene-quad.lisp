(in-package :graphene-test)

(def-suite graphene-quad :in graphene-suite)
(in-suite graphene-quad)

;;; --- Types and Values -------------------------------------------------------

;;;     graphene_quad_t

;;; --- Macros -----------------------------------------------------------------

(test with-graphene-quad.1
  (with-graphene-quad (q)
    (is (pointerp q))))

(test with-graphene-quad.2
  (with-graphene-quads (p1 p2 p3 p4)
    (with-graphene-quad (q p1 p2 p3 p4)
      (is (pointerp q)))))

(test with-graphene-quad.3
  (with-graphene-rect (r 1 2 3 4)
    (with-graphene-quad (q r)
      (is (pointerp q)))))

;;; --- Functions --------------------------------------------------------------

;;;     graphene_quad_alloc
;;;     graphene_quad_free

(test quad-alloc
  (let ((quad nil))
    (is (pointerp (setf quad (quad-alloc))))
    (is-false (quad-free quad))))

;;;     graphene_quad_init
;;;     graphene_quad_init_from_rect

;;;     graphene_quad_init_from_points

(test quad-init-from-points
  (with-graphene-points (p0 p1 p2 p3)
    (with-graphene-quad (q)
      (is (pointer-eq q (quad-init-from-points q (list p0 p1 p2 p3)))))))

;;;     graphene_quad_contains

(test quad-contains
  (with-graphene-points ((p0 1 2) (p1 4 2) (p2 4 6) (p3 1 6) (pt1 3 4) (pt2 3 10))
    (with-graphene-quad (q p0 p1 p2 p3)
      (is-true (quad-contains q pt1))
      (is-false (quad-contains q pt2)))))

;;;     graphene_quad_bounds

(test quad-bounds
  (with-graphene-points ((p0 1 2) (p1 4 2) (p2 4 6) (p3 1 6))
    (with-graphene-rect (result)
      (with-graphene-quad (q p0 p1 p2 p3)
        (is (pointer-eq result (quad-bounds q result)))
        (is (= 1.0 (rect-x result)))
        (is (= 2.0 (rect-y result)))
        (is (= 3.0 (rect-width result)))
        (is (= 4.0 (rect-height result)))))))

;;;     graphene_quad_get_point

(test quad-point
  (with-graphene-point (result)
    (with-graphene-rect (r 1 2 3 4)
      (with-graphene-quad (q r)
        (is (pointer-eq result (quad-point q 0 result)))
        (is (= 1.0 (point-x result)))
        (is (= 2.0 (point-y result)))
        (is (pointer-eq result (quad-point q 1 result)))
        (is (= 4.0 (point-x result)))
        (is (= 2.0 (point-y result)))
        (is (pointer-eq result (quad-point q 2 result)))
        (is (= 4.0 (point-x result)))
        (is (= 6.0 (point-y result)))
        (is (pointer-eq result (quad-point q 3 result)))
        (is (= 1.0 (point-x result)))
        (is (= 6.0 (point-y result)))))))

;;; 2022-9-20
