(in-package :graphene-test)

(def-suite graphene-plane :in graphene-suite)
(in-suite graphene-plane)

;;; --- Types and Values -------------------------------------------------------

;;;     graphene_plane_t

;;; --- Macros -----------------------------------------------------------------

(test with-graphene-plane.1
  (graphene:with-graphene-vec3 (normal)
    (graphene:with-graphene-plane (plane)
      (is (cffi:pointerp plane))
      (is (equal '(0.0 0.0 0.0) 
                 (graphene:vec3-to-float (graphene:plane-normal plane normal))))
      (is (= 0.0 (graphene:plane-constant plane))))))


;;; --- Functions --------------------------------------------------------------

;;;     graphene_plane_alloc
;;;     graphene_plane_free
;;;     graphene_plane_init
;;;     graphene_plane_init_from_vec4
;;;     graphene_plane_init_from_plane
;;;     graphene_plane_init_from_point
;;;     graphene_plane_init_from_points
;;;     graphene_plane_normalize
;;;     graphene_plane_negate
;;;     graphene_plane_equal
;;;     graphene_plane_distance
;;;     graphene_plane_transform
;;;     graphene_plane_get_normal
;;;     graphene_plane_get_constant

;;; 2022-9-25
