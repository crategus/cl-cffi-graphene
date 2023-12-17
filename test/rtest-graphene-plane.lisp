(in-package :graphene-test)

(def-suite graphene-plane :in graphene-suite)
(in-suite graphene-plane)

;;; --- Types and Values -------------------------------------------------------

;;;     graphene_plane_t

;;; --- Macros -----------------------------------------------------------------

(test graphene-with-plane.1
  (graphene:with-vec3 (normal)
    (graphene:with-plane (plane)
      (is (cffi:pointerp plane))
      (is (equal '(0.0 0.0 0.0)
                 (graphene:vec3-to-float (graphene:plane-normal plane normal))))
      (is (= 0.0 (graphene:plane-constant plane))))))

;;; --- Functions --------------------------------------------------------------

;;;     graphene_plane_alloc
;;;     graphene_plane_free

(test graphene-plane-alloc/free
  (let (plane)
    (is (cffi:pointerp (setf plane (graphene:plane-alloc))))
    (is-false (graphene:plane-free plane))))

;;;     graphene_plane_init

(test graphene-plane-init.1
  (let ((plane (graphene:plane-alloc)))
    (graphene:with-vec3s ((normal 0.0 1.0 0.0) result)
      (graphene:plane-init plane normal 1.0)
      (is (graphene:plane-equal normal
                                (graphene:plane-normal plane result)))
      (is (= 1.0 (graphene:plane-constant plane)))
      (graphene:plane-free plane))))

;;FIXME: This test can fail, there is something wrong with the implemenation
(test graphene-plane-init.2
  (let ((plane (graphene:plane-alloc)))
    (graphene:with-vec3s ((normal 1.0 0.0 0.0) result)
      (is (cffi:pointer-eq plane (graphene:plane-init plane nil 1.0)))
      (is (graphene:plane-equal normal
                                (graphene:plane-normal plane result)))
      (is (= 1.0 (graphene:plane-constant plane)))
      (is-false (graphene:plane-free plane)))))

;;;     graphene_plane_init_from_vec4

(test graphene-plane-init-from-vec4
  (let ((plane (graphene:plane-alloc)))
    (graphene:with-objects ((vector graphene:vec4-t 0.0 1.0 0.0 1.0)
                            (check graphene:vec3-t 0.0 1.0 0.0)
                            (result graphene:vec3-t))
      (is (cffi:pointer-eq plane
                           (graphene:plane-init-from-vec4 plane vector)))
      (is (graphene:vec3-equal check
                               (graphene:plane-normal plane result)))
      (is (= 1.0 (graphene:plane-constant plane)))
      (is-false (graphene:plane-free plane)))))

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

(test graphene-normal/constant
  (graphene:with-objects ((normal graphene:vec3-t 1.0 0.0 0.0)
                          (plane graphene:plane-t normal 1.0)
                          (result graphene:vec3-t))

    (is (cffi:pointer-eq result
                         (graphene:plane-normal plane result)))
    (is (graphene:vec3-equal result normal))
    (is (= 1.0 (graphene:plane-constant plane)))))

;;; 2023-12-7
