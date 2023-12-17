(in-package :graphene-test)

(def-suite graphene-frustum :in graphene-suite)
(in-suite graphene-frustum)

;;; --- Types and Values -------------------------------------------------------

;;;     graphene_frustum_t

;;; --- Functions --------------------------------------------------------------

;;;     graphene_frustum_alloc
;;;     graphene_frustum_free
;;;     graphene_frustum_init
;;;     graphene_frustum_init_from_frustum
;;;     graphene_frustum_init_from_matrix

;;;     graphene_frustum_get_planes

(test graphene-frustum-planes
  (graphene:with-objects ((vec0 graphene:vec3-t 1 0 0)
                          (vec1 graphene:vec3-t 1 0 0)
                          (vec2 graphene:vec3-t 1 0 0)
                          (vec3 graphene:vec3-t 1 0 0)
                          (vec4 graphene:vec3-t 1 0 0)
                          (vec5 graphene:vec3-t 1 0 0)
                          (normal graphene:vec3-t)
                          (plane0 graphene:plane-t vec0 -1)
                          (plane1 graphene:plane-t vec1  1)
                          (plane2 graphene:plane-t vec2  2)
                          (plane3 graphene:plane-t vec3  3)
                          (plane4 graphene:plane-t vec4  4)
                          (plane5 graphene:plane-t vec5  5)
                          (plane6 graphene:plane-t)
                          (plane7 graphene:plane-t)
                          (plane8 graphene:plane-t)
                          (plane9 graphene:plane-t)
                          (plane10 graphene:plane-t)
                          (plane11 graphene:plane-t))
    ;; Create and initialize the frustum
    (graphene:with-frustum (frustum plane0 plane1 plane2 plane3 plane4 plane5)
      ;; Direct access of the array of planes
      (cffi:with-foreign-object (planes-ar '(:struct graphene:plane-t) 6)
        (is-false (graphene::%frustum-planes frustum planes-ar))
        ;; First plane
        (is (cffi:pointer-eq plane6
                             (graphene:plane-init-from-plane plane6 planes-ar)))
        (is (= -1.0 (graphene:plane-constant plane6)))
        (is (cffi:pointer-eq normal (graphene:plane-normal plane6 normal)))
        (is (equal '(1.0 0.0 0.0) (graphene:vec3-to-float normal)))
        ;; Access the second plane
        (is (cffi:pointerp (cffi:incf-pointer planes-ar 32))) ; size of 8 float
        (is (cffi:pointer-eq plane7
                             (graphene:plane-init-from-plane plane7 planes-ar)))
        (is (= 1.0 (graphene:plane-constant plane7)))
        (is (cffi:pointer-eq normal (graphene:plane-normal plane7 normal)))
        (is (equal '(1.0 0.0 0.0) (graphene:vec3-to-float normal))))
      ;; Now check the implementation
      (is (every #'cffi:pointerp
                 (graphene:frustum-planes frustum
                                          (list plane6 plane7 plane8
                                                plane9 plane10 plane11))))
      (is (= -1 (graphene:plane-constant plane6)))
      (is (equal '(1.0 0.0 0.0)
                 (graphene:vec3-to-float (graphene:plane-normal plane6 normal))))
)))

;;;     graphene_frustum_contains_point
;;;     graphene_frustum_intersects_sphere
;;;     graphene_frustum_intersects_box
;;;     graphene_frustum_equal

;;; 2023-12-10
