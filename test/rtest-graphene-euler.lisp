(in-package :graphene-test)

(def-suite graphene-euler :in graphene-suite)
(in-suite graphene-euler)

;;; --- Types and Values -------------------------------------------------------

;;;     graphene_euler_t
;;;     graphene_euler_order_t

;;; --- Macros -----------------------------------------------------------------

(test with-graphene-euler.1
  (with-graphene-euler (euler)
    (is (pointerp euler))
))

(test with-graphene-euler.2
  (with-graphene-euler (euler 1 2 3)
    (is (pointerp euler))
))

(test with-graphene-euler.3
  (with-graphene-euler (euler 1 2 3 :SXYZ)
    (is (pointerp euler))
))

(test with-graphene-euler.4
  (with-graphene-matrix (matrix)
    (with-graphene-euler (euler matrix :SXYZ)
      (is (pointerp euler))
)))

(test with-graphene-euler.5
  (with-graphene-quaternion (quaternion)
    (is (pointer-eq quaternion (quaternion-init-identity quaternion)))
    (with-graphene-euler (euler quaternion :SXYZ)
      (is (pointerp euler))
)))

(test with-graphene-euler.6
  (with-graphene-vec3 (vector 1 2 3)
    (with-graphene-euler (euler vector :SXYZ)
      (is (pointerp euler))
)))

(test with-graphene-euler.7
  (with-graphene-eulers (euler (euler1 euler))
    (is (pointerp euler))
    (is (pointerp euler1))))

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

;;; 2022-9-24
