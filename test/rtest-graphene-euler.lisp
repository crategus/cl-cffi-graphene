(in-package :graphene-test)

(def-suite graphene-euler :in graphene-suite)
(in-suite graphene-euler)

;;; --- Types and Values -------------------------------------------------------

;;;     graphene_euler_t
;;;     graphene_euler_order_t

;;; --- Macros -----------------------------------------------------------------

(test graphene-with-euler.1
  (graphene:with-euler (euler)
    (is (cffi:pointerp euler))
))

(test graphene-with-euler.2
  (graphene:with-euler (euler 1 2 3)
    (is (cffi:pointerp euler))
))

(test graphene-with-euler.3
  (graphene:with-euler (euler 1 2 3 :SXYZ)
    (is (cffi:pointerp euler))
))

(test graphene-with-euler.4
  (graphene:with-matrix (matrix)
    (graphene:with-euler (euler matrix :SXYZ)
      (is (cffi:pointerp euler))
)))

(test graphene-with-euler.5
  (graphene:with-quaternion (quaternion)
    (is (cffi:pointer-eq quaternion (graphene:quaternion-init-identity quaternion)))
    (graphene:with-euler (euler quaternion :SXYZ)
      (is (cffi:pointerp euler))
)))

(test graphene-with-euler.6
  (graphene:with-vec3 (vector 1 2 3)
    (graphene:with-euler (euler vector :SXYZ)
      (is (cffi:pointerp euler))
)))

(test graphene-with-euler.7
  (graphene:with-eulers (euler (euler1 euler))
    (is (cffi:pointerp euler))
    (is (cffi:pointerp euler1))))

;;; --- Functions --------------------------------------------------------------

;;;     graphene_euler_alloc
;;;     graphene_euler_free
;;;     graphene_euler_init
;;;     graphene_euler_init_with_order
;;;     graphene_euler_init_from_matrix
;;;     graphene_euler_init_from_quaternion
;;;     graphene_euler_init_from_vec3
;;;     graphene_euler_init_from_euler
;;;     graphene_euler_init_from_radians
;;;     graphene_euler_equal
;;;     graphene_euler_get_x
;;;     graphene_euler_get_y
;;;     graphene_euler_get_z
;;;     graphene_euler_get_order
;;;     graphene_euler_get_alpha
;;;     graphene_euler_get_beta
;;;     graphene_euler_get_gamma
;;;     graphene_euler_to_vec3
;;;     graphene_euler_to_matrix
;;;     graphene_euler_to_quaternion
;;;     graphene_euler_reorder

;;; 2023-12-10
