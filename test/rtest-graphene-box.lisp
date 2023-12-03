(in-package :graphene-test)

(def-suite graphene-box :in graphene-suite)
(in-suite graphene-box)

;;; --- Types and Values -------------------------------------------------------

;;;     graphene_box_t

;;; --- Macros -----------------------------------------------------------------

(test with-box.1
  (graphene:with-box (box)
    (is (cffi:pointerp box))))

(test with-box.2
  (graphene:with-point3ds ((p1 0 0 0) (p2 1 1 1))
    (graphene:with-box (box p1 p2)
      (is (cffi:pointerp box)))))

(test with-box.3
  (graphene:with-point3ds ((p1 0 0 0) (p2 1 1 1))
    (graphene:with-box (box (p1 graphene:point3d-t) p2)
      (is (cffi:pointerp box)))))

(test with-box.4
  (graphene:with-point3ds ((p1 0 0 0) (p2 1 1 1))
    (graphene:with-box (box (p1 graphene:point3d-t) (p2 graphene:point3d-t))
      (is (cffi:pointerp box)))))

(test with-box.5
  (graphene:with-vec3s ((v1 0 0 0) (v2 1 1 1))
    (graphene:with-box (box (v1 graphene:vec3-t) v2)
      (is (cffi:pointerp box)))))

(test with-box.6
  (graphene:with-vec3s ((v1 0 0 0) (v2 1 1 1))
    (graphene:with-box (box (v1 graphene:vec3-t) (v2 graphene:vec3-t))
      (is (cffi:pointerp box)))))

(test with-boxes.1
  (graphene:with-point3ds ((p1 0 0 0) (p2 1 1 1))
    (graphene:with-boxes (box (box1 p1 p2) (box2 box1))
      (is (cffi:pointerp box))
      (is (cffi:pointerp box1))
      (is (cffi:pointerp box2))
      (is (graphene:box-equal box1 box2)))))

(test with-boxes.2
  (graphene:with-vec3s ((v1 0 0 0) (v2 1 1 1))
    (graphene:with-boxes (box (box1 v1 v2) (box2 box1))
      (is (cffi:pointerp box))
      (is (cffi:pointerp box1))
      (is (cffi:pointerp box2))
      (is (graphene:box-equal box1 box2)))))

;;; --- Functions --------------------------------------------------------------

;;;     graphene_box_alloc
;;;     graphene_box_free

(test box-alloc/free
  (let ((box nil))
    (is (cffi:pointerp (setf box (graphene:box-alloc))))
    (is-false (graphene:box-free box))))

;;;     graphene_box_init

(test box-init
  (let ((box (graphene:box-alloc)))
    (graphene:with-point3ds ((p1 0 0 0) (p2 1 1 1))
      (is (cffi:pointer-eq box (graphene:box-init box p1 p2))))
    (graphene:box-free box)))

;;;     graphene_box_init_from_box

(test box-init-from-box
  (let ((box1 (graphene:box-alloc))
        (box2 (graphene:box-alloc)))
    (graphene:with-point3ds ((p1 0 0 0) (p2 1 1 1))
      (is (cffi:pointer-eq box2 (graphene:box-init box2 p1 p2)))
      (is (cffi:pointer-eq box1 (graphene:box-init-from-box box1 box2))))
    (graphene:box-free box1)
    (graphene:box-free box2)))

;;;     graphene_box_init_from_points

(test box-init-from-points
  (let ((box (graphene:box-alloc)))
    (graphene:with-point3ds ((p1 0 0 0) (p2 1 1 1))
      (is (cffi:pointer-eq box
                           (graphene:box-init-from-points box (list p1 p2)))))
    (graphene:box-free box)))

;;;     graphene_box_init_from_vec3

(test box-init-from-vec3
  (let ((box (graphene:box-alloc)))
    (graphene:with-vec3s ((v1 0 0 0) (v2 1 1 1))
      (is (cffi:pointer-eq box (graphene:box-init-from-vec3 box v1 v2))))
    (graphene:box-free box)))

;;;     graphene_box_init_from_vectors

(test box-init-from-vectors
  (let ((box (graphene:box-alloc)))
    (graphene:with-vec3s ((v1 0 0 0) (v2 1 1 1))
      (is (cffi:pointer-eq box (graphene:box-init-from-vectors box (list v1 v2)))))
    (graphene:box-free box)))

;;;     graphene_box_equal

(test box-equal
  (graphene:with-point3ds ((p1 0 0 0) (p2 1 1 1))
    (graphene:with-boxes (box (box1 p1 p2) (box2 box1))
      (is (graphene:box-equal box1 box2))
      (is (not (graphene:box-equal box box1)))
      (is (not (graphene:box-equal box box2))))))

;;;     graphene_box_expand

(test box-expand
  (graphene:with-point3ds ((min 0 0 0) (max 1 1 1) (point 2 2 2))
    (graphene:with-boxes ((box min max) result)

      (is (cffi:pointer-eq result (graphene:box-expand box point result)))

      (is (= 0.0 (graphene:point3d-x (graphene:box-min box min))))
      (is (= 0.0 (graphene:point3d-y (graphene:box-min box min))))
      (is (= 0.0 (graphene:point3d-z (graphene:box-min box min))))
      (is (= 1.0 (graphene:point3d-x (graphene:box-max box max))))
      (is (= 1.0 (graphene:point3d-y (graphene:box-max box max))))
      (is (= 1.0 (graphene:point3d-z (graphene:box-max box max))))

      (is (= 0.0 (graphene:point3d-x (graphene:box-min result min))))
      (is (= 0.0 (graphene:point3d-y (graphene:box-min result min))))
      (is (= 0.0 (graphene:point3d-z (graphene:box-min result min))))
      (is (= 2.0 (graphene:point3d-x (graphene:box-max result max))))
      (is (= 2.0 (graphene:point3d-y (graphene:box-max result max))))
      (is (= 2.0 (graphene:point3d-z (graphene:box-max result max)))))))

;;;     graphene_box_expand_scalar

(test box-expand-scalar
  (graphene:with-point3ds ((min 0 0 0) (max 1 1 1) point)
    (graphene:with-boxes ((box min max) result)

      (is (cffi:pointer-eq result (graphene:box-expand-scalar box 2.0 result)))

      (is (= 0.0 (graphene:point3d-x (graphene:box-min box point))))
      (is (= 0.0 (graphene:point3d-y (graphene:box-min box point))))
      (is (= 0.0 (graphene:point3d-z (graphene:box-min box point))))
      (is (= 1.0 (graphene:point3d-x (graphene:box-max box point))))
      (is (= 1.0 (graphene:point3d-y (graphene:box-max box point))))
      (is (= 1.0 (graphene:point3d-z (graphene:box-max box point))))

      (is (= -2.0 (graphene:point3d-x (graphene:box-min result point))))
      (is (= -2.0 (graphene:point3d-y (graphene:box-min result point))))
      (is (= -2.0 (graphene:point3d-z (graphene:box-min result point))))
      (is (=  3.0 (graphene:point3d-x (graphene:box-max result point))))
      (is (=  3.0 (graphene:point3d-y (graphene:box-max result point))))
      (is (=  3.0 (graphene:point3d-z (graphene:box-max result point)))))))

;;;     graphene_box_expand_vec3

(test box-expand-vec3
  (graphene:with-point3ds ((min 0 0 0) (max 1 1 1))
    (graphene:with-vec3 (vector 2 2 2)
      (graphene:with-boxes ((box min max) result)

        (is (cffi:pointer-eq result (graphene:box-expand-vec3 box vector result)))

        (is (= 0.0 (graphene:point3d-x (graphene:box-min box min))))
        (is (= 0.0 (graphene:point3d-y (graphene:box-min box min))))
        (is (= 0.0 (graphene:point3d-z (graphene:box-min box min))))
        (is (= 1.0 (graphene:point3d-x (graphene:box-max box max))))
        (is (= 1.0 (graphene:point3d-y (graphene:box-max box max))))
        (is (= 1.0 (graphene:point3d-z (graphene:box-max box max))))

        (is (= 0.0 (graphene:point3d-x (graphene:box-min result min))))
        (is (= 0.0 (graphene:point3d-y (graphene:box-min result min))))
        (is (= 0.0 (graphene:point3d-z (graphene:box-min result min))))
        (is (= 2.0 (graphene:point3d-x (graphene:box-max result max))))
        (is (= 2.0 (graphene:point3d-y (graphene:box-max result max))))
        (is (= 2.0 (graphene:point3d-z (graphene:box-max result max))))))))

;;;     graphene_box_get_min
;;;     graphene_box_get_max

(test box-min/max.1
  (graphene:with-point3ds ((min 0 1 2) (max 3 4 5) result)
    (graphene:with-box (box min max)

      (is (not (cffi:pointer-eq min (graphene:box-min box result))))
      (is (not (cffi:pointer-eq max (graphene:box-max box result))))

      (is (cffi:pointer-eq result (graphene:box-min box result)))
      (is (cffi:pointer-eq result (graphene:box-max box result)))

      (is (graphene:point3d-equal min (graphene:box-min box result)))
      (is (graphene:point3d-equal max (graphene:box-max box result)))

      (is (= 0.0 (graphene:point3d-x (graphene:box-min box result))))
      (is (= 1.0 (graphene:point3d-y (graphene:box-min box result))))
      (is (= 2.0 (graphene:point3d-z (graphene:box-min box result))))
      (is (= 3.0 (graphene:point3d-x (graphene:box-max box result))))
      (is (= 4.0 (graphene:point3d-y (graphene:box-max box result))))
      (is (= 5.0 (graphene:point3d-z (graphene:box-max box result)))))))

(test box-min/max.2
  (graphene:with-point3ds ((min 0 1 2) (max 3 4 5))
    (graphene:with-box (box min max)

      (is (cffi:pointer-eq min (graphene:box-min box min)))
      (is (cffi:pointer-eq max (graphene:box-max box max)))

      (is (graphene:point3d-equal min (graphene:box-min box min)))
      (is (graphene:point3d-equal max (graphene:box-max box max)))

      (is (= 0.0 (graphene:point3d-x (graphene:box-min box min))))
      (is (= 1.0 (graphene:point3d-y (graphene:box-min box min))))
      (is (= 2.0 (graphene:point3d-z (graphene:box-min box min))))
      (is (= 3.0 (graphene:point3d-x (graphene:box-max box max))))
      (is (= 4.0 (graphene:point3d-y (graphene:box-max box max))))
      (is (= 5.0 (graphene:point3d-z (graphene:box-max box max)))))))

;;;     graphene_box_get_center

(test box-center
  (graphene:with-point3ds ((min 0 0 0) (max 1 1 1) point)
    (graphene:with-box (box min max)
      (is (cffi:pointer-eq point (graphene:box-center box point)))
      (is (= 0.5 (graphene:point3d-x point)))
      (is (= 0.5 (graphene:point3d-y point)))
      (is (= 0.5 (graphene:point3d-z point))))))

;;;     graphene_box_get_depth
;;;     graphene_box_get_height
;;;     graphene_box_get_width

(test box-depth/height/width
  (graphene:with-point3ds ((min 0 0 0) (max 1 1 1))
    (graphene:with-box (box min max)

      (is (= 1.0 (graphene:box-depth box)))
      (is (= 1.0 (graphene:box-height box)))
      (is (= 1.0 (graphene:box-width box))))))

;;;     graphene_box_get_size

(test box-size
  (graphene:with-point3ds ((min 1 2 3) (max 4 4 4))
    (graphene:with-vec3 (vector)
      (graphene:with-box (box min max)

        (is (cffi:pointer-eq vector (graphene:box-size box vector)))

        (is (equal '(3.0 2.0 1.0)
                   (graphene:vec3-to-float (graphene:box-size box vector))))
        (is (equal (list (graphene:box-width box) (graphene:box-height box) (graphene:box-depth box))
                   (graphene:vec3-to-float (graphene:box-size box vector))))))))

;;;     graphene_box_get_bounding_sphere

(test box-bounding-sphere
  (graphene:with-sphere (sphere)
    (graphene:with-point3ds ((min 0 0 0) (max 1 1 1) center)
      (graphene:with-box (box min max)

      (is (cffi:pointer-eq sphere (graphene:box-bounding-sphere box sphere)))

      (is (cffi:pointer-eq center (graphene:sphere-center sphere center)))
      (is (= 0.5 (graphene:point3d-x (graphene:sphere-center sphere center))))
      (is (= 0.5 (graphene:point3d-y (graphene:sphere-center sphere center))))
      (is (= 0.5 (graphene:point3d-z (graphene:sphere-center sphere center))))

      (is (= 0.8660254 (graphene:sphere-radius sphere)))))))

;;;     graphene_box_get_vertices

(test box-vertices
  (graphene:with-vec3s (v0 v1 v2 v3 v4 v5 v6 v7)
    (graphene:with-point3ds ((min 0 0 0) (max 1 1 1))
      (graphene:with-box (box min max)
        (is (equal '((0.0 0.0 0.0)
                     (0.0 0.0 1.0)
                     (0.0 1.0 0.0)
                     (0.0 1.0 1.0)
                     (1.0 0.0 0.0)
                     (1.0 0.0 1.0)
                     (1.0 1.0 0.0)
                     (1.0 1.0 1.0))
                    (mapcar #'graphene:vec3-to-float
                           (graphene:box-vertices box
                                         (list v0 v1 v2 v3 v4 v5 v6 v7)))))))))

;;;     graphene_box_union
;;;     graphene_box_intersection
;;;     graphene_box_contains_box
;;;     graphene_box_contains_point

;;;     graphene_box_zero

(test box-zero
  (graphene:with-point3ds (min max)
    (graphene:with-box (box)

      (is (= 0.0 (graphene:point3d-x (graphene:box-min (graphene:box-zero) min))))
      (is (= 0.0 (graphene:point3d-y (graphene:box-min (graphene:box-zero) min))))
      (is (= 0.0 (graphene:point3d-z (graphene:box-min (graphene:box-zero) min))))
      (is (= 0.0 (graphene:point3d-x (graphene:box-max (graphene:box-zero) max))))
      (is (= 0.0 (graphene:point3d-y (graphene:box-max (graphene:box-zero) max))))
      (is (= 0.0 (graphene:point3d-z (graphene:box-max (graphene:box-zero) max))))

      (is (cffi:pointer-eq box (graphene:box-init-from-box box (graphene:box-zero))))

      (is (= 0.0 (graphene:point3d-x (graphene:box-min box min))))
      (is (= 0.0 (graphene:point3d-y (graphene:box-min box min))))
      (is (= 0.0 (graphene:point3d-z (graphene:box-min box min))))
      (is (= 0.0 (graphene:point3d-x (graphene:box-max box max))))
      (is (= 0.0 (graphene:point3d-y (graphene:box-max box max))))
      (is (= 0.0 (graphene:point3d-z (graphene:box-max box max)))))))

;;;     graphene_box_one

(test box-one
  (graphene:with-point3ds (min max)
    (graphene:with-box (box)

      (is (= 0.0 (graphene:point3d-x (graphene:box-min (graphene:box-one) min))))
      (is (= 0.0 (graphene:point3d-y (graphene:box-min (graphene:box-one) min))))
      (is (= 0.0 (graphene:point3d-z (graphene:box-min (graphene:box-one) min))))
      (is (= 1.0 (graphene:point3d-x (graphene:box-max (graphene:box-one) max))))
      (is (= 1.0 (graphene:point3d-y (graphene:box-max (graphene:box-one) max))))
      (is (= 1.0 (graphene:point3d-z (graphene:box-max (graphene:box-one) max))))

      (is (cffi:pointer-eq box (graphene:box-init-from-box box (graphene:box-one))))

      (is (= 0.0 (graphene:point3d-x (graphene:box-min box min))))
      (is (= 0.0 (graphene:point3d-y (graphene:box-min box min))))
      (is (= 0.0 (graphene:point3d-z (graphene:box-min box min))))
      (is (= 1.0 (graphene:point3d-x (graphene:box-max box max))))
      (is (= 1.0 (graphene:point3d-y (graphene:box-max box max))))
      (is (= 1.0 (graphene:point3d-z (graphene:box-max box max)))))))

;;;     graphene_box_minus_one

(test box-minus-one
  (graphene:with-point3ds (min max)
    (graphene:with-box (box)

      (is (= -1.0 (graphene:point3d-x (graphene:box-min (graphene:box-minus-one) min))))
      (is (= -1.0 (graphene:point3d-y (graphene:box-min (graphene:box-minus-one) min))))
      (is (= -1.0 (graphene:point3d-z (graphene:box-min (graphene:box-minus-one) min))))
      (is (=  0.0 (graphene:point3d-x (graphene:box-max (graphene:box-minus-one) max))))
      (is (=  0.0 (graphene:point3d-y (graphene:box-max (graphene:box-minus-one) max))))
      (is (=  0.0 (graphene:point3d-z (graphene:box-max (graphene:box-minus-one) max))))

      (is (cffi:pointer-eq box (graphene:box-init-from-box box (graphene:box-minus-one))))

      (is (= -1.0 (graphene:point3d-x (graphene:box-min box min))))
      (is (= -1.0 (graphene:point3d-y (graphene:box-min box min))))
      (is (= -1.0 (graphene:point3d-z (graphene:box-min box min))))
      (is (=  0.0 (graphene:point3d-x (graphene:box-max box max))))
      (is (=  0.0 (graphene:point3d-y (graphene:box-max box max))))
      (is (=  0.0 (graphene:point3d-z (graphene:box-max box max)))))))

;;;     graphene_box_one_minus_one

(test box-one-minus-one
  (graphene:with-point3ds (min max)
    (graphene:with-box (box)

      (is (= -1.0 (graphene:point3d-x (graphene:box-min (graphene:box-one-minus-one) min))))
      (is (= -1.0 (graphene:point3d-y (graphene:box-min (graphene:box-one-minus-one) min))))
      (is (= -1.0 (graphene:point3d-z (graphene:box-min (graphene:box-one-minus-one) min))))
      (is (=  1.0 (graphene:point3d-x (graphene:box-max (graphene:box-one-minus-one) max))))
      (is (=  1.0 (graphene:point3d-y (graphene:box-max (graphene:box-one-minus-one) max))))
      (is (=  1.0 (graphene:point3d-z (graphene:box-max (graphene:box-one-minus-one) max))))

      (is (cffi:pointer-eq box (graphene:box-init-from-box box (graphene:box-one-minus-one))))

      (is (= -1.0 (graphene:point3d-x (graphene:box-min box min))))
      (is (= -1.0 (graphene:point3d-y (graphene:box-min box min))))
      (is (= -1.0 (graphene:point3d-z (graphene:box-min box min))))
      (is (=  1.0 (graphene:point3d-x (graphene:box-max box max))))
      (is (=  1.0 (graphene:point3d-y (graphene:box-max box max))))
      (is (=  1.0 (graphene:point3d-z (graphene:box-max box max)))))))

;;;     graphene_box_empty
;;;     graphene_box_infinite

;;; 2023-12-2