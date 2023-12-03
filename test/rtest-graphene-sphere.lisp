(in-package :graphene-test)

(def-suite graphene-sphere :in graphene-suite)
(in-suite graphene-sphere)

;;; --- Types and Values -------------------------------------------------------

;;;     graphene_sphere_t

;;; --- Macros -----------------------------------------------------------------

(test with-sphere
  (graphene:with-point3d (center 0.0 0.0 0.0)
    (graphene:with-sphere (sphere center 1.0)

)))

;;; --- Functions --------------------------------------------------------------

;;;     graphene_sphere_alloc
;;;     graphene_sphere_free

(test sphere-alloc/free
  (let ((sphere nil))
    (is (cffi:pointerp (setf sphere (graphene:sphere-alloc))))
    (is-false (graphene:sphere-free sphere))))

;;;     graphene_sphere_init

(test sphere-init
  (let ((sphere (graphene:sphere-alloc)))
    (graphene:with-point3d (center 0 0 0)
      (is (cffi:pointer-eq sphere (graphene:sphere-init sphere center 1))))
    (graphene:sphere-free sphere)))

;;;     graphene_sphere_init_from_points
;;;     graphene_sphere_init_from_vectors

;;;     graphene_sphere_get_center
;;;     graphene_sphere_get_radius

(test sphere-center/radius
  (graphene:with-vec3 (vector)
    (graphene:with-point3ds ((center 1.0 2.0 3.0) point)
      (graphene:with-sphere (sphere center 1.0)

        (is (cffi:pointer-eq point (graphene:sphere-center sphere point)))
        (is (= 1.0 (graphene:sphere-radius sphere)))

        (is (equal '(1.0 2.0 3.0)
                   (graphene:vec3-to-float
                       (graphene:point3d-to-vec3 (graphene:sphere-center sphere point) vector))))

))))

;;;     graphene_sphere_get_bounding_box
;;;     graphene_sphere_is_empty
;;;     graphene_sphere_distance
;;;     graphene_sphere_contains_point
;;;     graphene_sphere_translate
;;;     graphene_sphere_equal

;;; 2022-9-24
