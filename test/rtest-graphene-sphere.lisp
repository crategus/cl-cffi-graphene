(in-package :graphene-test)

(def-suite graphene-sphere :in graphene-suite)
(in-suite graphene-sphere)

;;; --- Types and Values -------------------------------------------------------

;;;     graphene_sphere_t

;;; --- Macros -----------------------------------------------------------------

(test with-graphene-sphere
  (with-graphene-point3d (center 0.0 0.0 0.0)
    (with-graphene-sphere (sphere center 1.0)

)))

;;; --- Functions --------------------------------------------------------------

;;;     graphene_sphere_alloc
;;;     graphene_sphere_free

(test sphere-alloc/free
  (let ((sphere nil))
    (is (pointerp (setf sphere (sphere-alloc))))
    (is-false (sphere-free sphere))))

;;;     graphene_sphere_init

(test sphere-init
  (let ((sphere (sphere-alloc)))
    (with-graphene-point3d (center 0 0 0)
      (is (pointer-eq sphere (sphere-init sphere center 1))))
    (sphere-free sphere)))

;;;     graphene_sphere_init_from_points
;;;     graphene_sphere_init_from_vectors

;;;     graphene_sphere_get_center
;;;     graphene_sphere_get_radius

(test sphere-center/radius
  (with-graphene-vec3 (vector)
    (with-graphene-point3ds ((center 1.0 2.0 3.0) point)
      (with-graphene-sphere (sphere center 1.0)

        (is (pointer-eq point (sphere-center sphere point)))
        (is (= 1.0 (sphere-radius sphere)))

        (is (equal '(1.0 2.0 3.0)
                   (vec3-to-float
                       (point3d-to-vec3 (sphere-center sphere point) vector))))

))))

;;;     graphene_sphere_get_bounding_box
;;;     graphene_sphere_is_empty
;;;     graphene_sphere_distance
;;;     graphene_sphere_contains_point
;;;     graphene_sphere_translate
;;;     graphene_sphere_equal

;;; 2022-9-24
