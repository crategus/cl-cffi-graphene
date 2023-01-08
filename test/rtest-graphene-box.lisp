(in-package :graphene-test)

(def-suite graphene-box :in graphene-suite)
(in-suite graphene-box)

;;; --- Types and Values -------------------------------------------------------

;;;     graphene_box_t

;;; --- Macros -----------------------------------------------------------------

(test with-graphene-box.1
  (with-graphene-box (box)
    (is (pointerp box))))

(test with-graphene-box.2
  (with-graphene-point3ds ((p1 0 0 0) (p2 1 1 1))
    (with-graphene-box (box p1 p2)
      (is (pointerp box)))))

(test with-graphene-box.3
  (with-graphene-point3ds ((p1 0 0 0) (p2 1 1 1))
    (with-graphene-box (box (p1 point3d-t) p2)
      (is (pointerp box)))))

(test with-graphene-box.4
  (with-graphene-point3ds ((p1 0 0 0) (p2 1 1 1))
    (with-graphene-box (box (p1 point3d-t) (p2 point3d-t))
      (is (pointerp box)))))

(test with-graphene-box.5
  (with-graphene-vec3s ((v1 0 0 0) (v2 1 1 1))
    (with-graphene-box (box (v1 vec3-t) v2)
      (is (pointerp box)))))

(test with-graphene-box.6
  (with-graphene-vec3s ((v1 0 0 0) (v2 1 1 1))
    (with-graphene-box (box (v1 vec3-t) (v2 vec3-t))
      (is (pointerp box)))))

(test with-graphene-boxes.1
  (with-graphene-point3ds ((p1 0 0 0) (p2 1 1 1))
    (with-graphene-boxes (box (box1 p1 p2) (box2 box1))
      (is (pointerp box))
      (is (pointerp box1))
      (is (pointerp box2))
      (is (box-equal box1 box2)))))

(test with-graphene-boxes.2
  (with-graphene-vec3s ((v1 0 0 0) (v2 1 1 1))
    (with-graphene-boxes (box (box1 v1 v2) (box2 box1))
      (is (pointerp box))
      (is (pointerp box1))
      (is (pointerp box2))
      (is (box-equal box1 box2)))))

;;; --- Functions --------------------------------------------------------------

;;;     graphene_box_alloc
;;;     graphene_box_free

(test box-alloc/free
  (let ((box nil))
    (is (pointerp (setf box (box-alloc))))
    (is-false (box-free box))))

;;;     graphene_box_init

(test box-init
  (let ((box (box-alloc)))
    (with-graphene-point3ds ((p1 0 0 0) (p2 1 1 1))
      (is (pointer-eq box (box-init box p1 p2))))
    (box-free box)))

;;;     graphene_box_init_from_box

(test box-init-from-box
  (let ((box1 (box-alloc))
        (box2 (box-alloc)))
    (with-graphene-point3ds ((p1 0 0 0) (p2 1 1 1))
      (is (pointer-eq box2 (box-init box2 p1 p2)))
      (is (pointer-eq box1 (box-init-from-box box1 box2))))
    (box-free box1)
    (box-free box2)))

;;;     graphene_box_init_from_points

(test box-init-from-points
  (let ((box (box-alloc)))
    (with-graphene-point3ds ((p1 0 0 0) (p2 1 1 1))
      (is (pointer-eq box (box-init-from-points box (list p1 p2)))))
    (box-free box)))

;;;     graphene_box_init_from_vec3

(test box-init-from-vec3
  (let ((box (box-alloc)))
    (with-graphene-vec3s ((v1 0 0 0) (v2 1 1 1))
      (is (pointer-eq box (box-init-from-vec3 box v1 v2))))
    (box-free box)))

;;;     graphene_box_init_from_vectors

(test box-init-from-vectors
  (let ((box (box-alloc)))
    (with-graphene-vec3s ((v1 0 0 0) (v2 1 1 1))
      (is (pointer-eq box (box-init-from-vectors box (list v1 v2)))))
    (box-free box)))

;;;     graphene_box_equal

(test box-equal
  (with-graphene-point3ds ((p1 0 0 0) (p2 1 1 1))
    (with-graphene-boxes (box (box1 p1 p2) (box2 box1))
      (is (box-equal box1 box2))
      (is (not (box-equal box box1)))
      (is (not (box-equal box box2))))))

;;;     graphene_box_expand

(test box-expand
  (with-graphene-point3ds ((min 0 0 0) (max 1 1 1) (point 2 2 2))
    (with-graphene-boxes ((box min max) result)

      (is (pointer-eq result (box-expand box point result)))

      (is (= 0.0 (point3d-x (box-min box min))))
      (is (= 0.0 (point3d-y (box-min box min))))
      (is (= 0.0 (point3d-z (box-min box min))))
      (is (= 1.0 (point3d-x (box-max box max))))
      (is (= 1.0 (point3d-y (box-max box max))))
      (is (= 1.0 (point3d-z (box-max box max))))

      (is (= 0.0 (point3d-x (box-min result min))))
      (is (= 0.0 (point3d-y (box-min result min))))
      (is (= 0.0 (point3d-z (box-min result min))))
      (is (= 2.0 (point3d-x (box-max result max))))
      (is (= 2.0 (point3d-y (box-max result max))))
      (is (= 2.0 (point3d-z (box-max result max)))))))

;;;     graphene_box_expand_scalar

(test box-expand-scalar
  (with-graphene-point3ds ((min 0 0 0) (max 1 1 1) point)
    (with-graphene-boxes ((box min max) result)

      (is (pointer-eq result (box-expand-scalar box 2.0 result)))

      (is (= 0.0 (point3d-x (box-min box point))))
      (is (= 0.0 (point3d-y (box-min box point))))
      (is (= 0.0 (point3d-z (box-min box point))))
      (is (= 1.0 (point3d-x (box-max box point))))
      (is (= 1.0 (point3d-y (box-max box point))))
      (is (= 1.0 (point3d-z (box-max box point))))

      (is (= -2.0 (point3d-x (box-min result point))))
      (is (= -2.0 (point3d-y (box-min result point))))
      (is (= -2.0 (point3d-z (box-min result point))))
      (is (=  3.0 (point3d-x (box-max result point))))
      (is (=  3.0 (point3d-y (box-max result point))))
      (is (=  3.0 (point3d-z (box-max result point)))))))

;;;     graphene_box_expand_vec3

(test box-expand-vec3
  (with-graphene-point3ds ((min 0 0 0) (max 1 1 1))
    (with-graphene-vec3 (vector 2 2 2)
      (with-graphene-boxes ((box min max) result)

        (is (pointer-eq result (box-expand-vec3 box vector result)))

        (is (= 0.0 (point3d-x (box-min box min))))
        (is (= 0.0 (point3d-y (box-min box min))))
        (is (= 0.0 (point3d-z (box-min box min))))
        (is (= 1.0 (point3d-x (box-max box max))))
        (is (= 1.0 (point3d-y (box-max box max))))
        (is (= 1.0 (point3d-z (box-max box max))))

        (is (= 0.0 (point3d-x (box-min result min))))
        (is (= 0.0 (point3d-y (box-min result min))))
        (is (= 0.0 (point3d-z (box-min result min))))
        (is (= 2.0 (point3d-x (box-max result max))))
        (is (= 2.0 (point3d-y (box-max result max))))
        (is (= 2.0 (point3d-z (box-max result max))))))))

;;;     graphene_box_get_min
;;;     graphene_box_get_max

(test box-min/max.1
  (with-graphene-point3ds ((min 0 1 2) (max 3 4 5) result)
    (with-graphene-box (box min max)

      (is (not (pointer-eq min (box-min box result))))
      (is (not (pointer-eq max (box-max box result))))

      (is (pointer-eq result (box-min box result)))
      (is (pointer-eq result (box-max box result)))

      (is (point3d-equal min (box-min box result)))
      (is (point3d-equal max (box-max box result)))

      (is (= 0.0 (point3d-x (box-min box result))))
      (is (= 1.0 (point3d-y (box-min box result))))
      (is (= 2.0 (point3d-z (box-min box result))))
      (is (= 3.0 (point3d-x (box-max box result))))
      (is (= 4.0 (point3d-y (box-max box result))))
      (is (= 5.0 (point3d-z (box-max box result)))))))

(test box-min/max.2
  (with-graphene-point3ds ((min 0 1 2) (max 3 4 5))
    (with-graphene-box (box min max)

      (is (pointer-eq min (box-min box min)))
      (is (pointer-eq max (box-max box max)))

      (is (point3d-equal min (box-min box min)))
      (is (point3d-equal max (box-max box max)))

      (is (= 0.0 (point3d-x (box-min box min))))
      (is (= 1.0 (point3d-y (box-min box min))))
      (is (= 2.0 (point3d-z (box-min box min))))
      (is (= 3.0 (point3d-x (box-max box max))))
      (is (= 4.0 (point3d-y (box-max box max))))
      (is (= 5.0 (point3d-z (box-max box max)))))))

;;;     graphene_box_get_center

(test box-center
  (with-graphene-point3ds ((min 0 0 0) (max 1 1 1) point)
    (with-graphene-box (box min max)
      (is (pointer-eq point (box-center box point)))
      (is (= 0.5 (point3d-x point)))
      (is (= 0.5 (point3d-y point)))
      (is (= 0.5 (point3d-z point))))))

;;;     graphene_box_get_depth
;;;     graphene_box_get_height
;;;     graphene_box_get_width

(test box-depth/height/width
  (with-graphene-point3ds ((min 0 0 0) (max 1 1 1))
    (with-graphene-box (box min max)

      (is (= 1.0 (box-depth box)))
      (is (= 1.0 (box-height box)))
      (is (= 1.0 (box-width box))))))

;;;     graphene_box_get_size

(test box-size
  (with-graphene-point3ds ((min 1 2 3) (max 4 4 4))
    (with-graphene-vec3 (vector)
      (with-graphene-box (box min max)

        (is (pointer-eq vector (box-size box vector)))

        (is (equal '(3.0 2.0 1.0)
                   (vec3-to-float (box-size box vector))))
        (is (equal (list (box-width box) (box-height box) (box-depth box))
                   (vec3-to-float (box-size box vector))))))))

;;;     graphene_box_get_bounding_sphere

(test box-bounding-sphere
  (with-graphene-sphere (sphere)
    (with-graphene-point3ds ((min 0 0 0) (max 1 1 1) center)
      (with-graphene-box (box min max)

      (is (pointer-eq sphere (box-bounding-sphere box sphere)))

      (is (pointer-eq center (sphere-center sphere center)))
      (is (= 0.5 (point3d-x (sphere-center sphere center))))
      (is (= 0.5 (point3d-y (sphere-center sphere center))))
      (is (= 0.5 (point3d-z (sphere-center sphere center))))

      (is (= 0.8660254 (sphere-radius sphere)))))))

;;;     graphene_box_get_vertices

(test box-vertices
  (with-graphene-vec3s (v0 v1 v2 v3 v4 v5 v6 v7)
    (with-graphene-point3ds ((min 0 0 0) (max 1 1 1))
      (with-graphene-box (box min max)
        (is (equal '((0.0 0.0 0.0)
                     (0.0 0.0 1.0)
                     (0.0 1.0 0.0)
                     (0.0 1.0 1.0)
                     (1.0 0.0 0.0)
                     (1.0 0.0 1.0)
                     (1.0 1.0 0.0)
                     (1.0 1.0 1.0))
                    (mapcar #'vec3-to-float
                           (box-vertices box
                                         (list v0 v1 v2 v3 v4 v5 v6 v7)))))))))

;;;     graphene_box_union
;;;     graphene_box_intersection
;;;     graphene_box_contains_box
;;;     graphene_box_contains_point

;;;     graphene_box_zero

(test box-zero
  (with-graphene-point3ds (min max)
    (with-graphene-box (box)

      (is (= 0.0 (point3d-x (box-min (box-zero) min))))
      (is (= 0.0 (point3d-y (box-min (box-zero) min))))
      (is (= 0.0 (point3d-z (box-min (box-zero) min))))
      (is (= 0.0 (point3d-x (box-max (box-zero) max))))
      (is (= 0.0 (point3d-y (box-max (box-zero) max))))
      (is (= 0.0 (point3d-z (box-max (box-zero) max))))

      (is (pointer-eq box (box-init-from-box box (box-zero))))

      (is (= 0.0 (point3d-x (box-min box min))))
      (is (= 0.0 (point3d-y (box-min box min))))
      (is (= 0.0 (point3d-z (box-min box min))))
      (is (= 0.0 (point3d-x (box-max box max))))
      (is (= 0.0 (point3d-y (box-max box max))))
      (is (= 0.0 (point3d-z (box-max box max)))))))

;;;     graphene_box_one

(test box-one
  (with-graphene-point3ds (min max)
    (with-graphene-box (box)

      (is (= 0.0 (point3d-x (box-min (box-one) min))))
      (is (= 0.0 (point3d-y (box-min (box-one) min))))
      (is (= 0.0 (point3d-z (box-min (box-one) min))))
      (is (= 1.0 (point3d-x (box-max (box-one) max))))
      (is (= 1.0 (point3d-y (box-max (box-one) max))))
      (is (= 1.0 (point3d-z (box-max (box-one) max))))

      (is (pointer-eq box (box-init-from-box box (box-one))))

      (is (= 0.0 (point3d-x (box-min box min))))
      (is (= 0.0 (point3d-y (box-min box min))))
      (is (= 0.0 (point3d-z (box-min box min))))
      (is (= 1.0 (point3d-x (box-max box max))))
      (is (= 1.0 (point3d-y (box-max box max))))
      (is (= 1.0 (point3d-z (box-max box max)))))))

;;;     graphene_box_minus_one

(test box-minus-one
  (with-graphene-point3ds (min max)
    (with-graphene-box (box)

      (is (= -1.0 (point3d-x (box-min (box-minus-one) min))))
      (is (= -1.0 (point3d-y (box-min (box-minus-one) min))))
      (is (= -1.0 (point3d-z (box-min (box-minus-one) min))))
      (is (=  0.0 (point3d-x (box-max (box-minus-one) max))))
      (is (=  0.0 (point3d-y (box-max (box-minus-one) max))))
      (is (=  0.0 (point3d-z (box-max (box-minus-one) max))))

      (is (pointer-eq box (box-init-from-box box (box-minus-one))))

      (is (= -1.0 (point3d-x (box-min box min))))
      (is (= -1.0 (point3d-y (box-min box min))))
      (is (= -1.0 (point3d-z (box-min box min))))
      (is (=  0.0 (point3d-x (box-max box max))))
      (is (=  0.0 (point3d-y (box-max box max))))
      (is (=  0.0 (point3d-z (box-max box max)))))))

;;;     graphene_box_one_minus_one

(test box-one-minus-one
  (with-graphene-point3ds (min max)
    (with-graphene-box (box)

      (is (= -1.0 (point3d-x (box-min (box-one-minus-one) min))))
      (is (= -1.0 (point3d-y (box-min (box-one-minus-one) min))))
      (is (= -1.0 (point3d-z (box-min (box-one-minus-one) min))))
      (is (=  1.0 (point3d-x (box-max (box-one-minus-one) max))))
      (is (=  1.0 (point3d-y (box-max (box-one-minus-one) max))))
      (is (=  1.0 (point3d-z (box-max (box-one-minus-one) max))))

      (is (pointer-eq box (box-init-from-box box (box-one-minus-one))))

      (is (= -1.0 (point3d-x (box-min box min))))
      (is (= -1.0 (point3d-y (box-min box min))))
      (is (= -1.0 (point3d-z (box-min box min))))
      (is (=  1.0 (point3d-x (box-max box max))))
      (is (=  1.0 (point3d-y (box-max box max))))
      (is (=  1.0 (point3d-z (box-max box max)))))))

;;;     graphene_box_empty
;;;     graphene_box_infinite

;;; 2022-9-22
