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
;;;     graphene_frustum_equal

(test graphene-with-frustum.1
  (graphene:with-frustum (frustum)
    (is (cffi:pointerp frustum))))

(test graphene-with-frustum.2
  (let ((left 0) (right 1) (bottom 0) (top 1) (znear 1) (zfar 2))
    (graphene:with-matrix (matrix)
      (graphene:matrix-init-frustum matrix left right bottom top znear zfar)
      (graphene:with-frustum (frustum (matrix graphene:matrix-t))
        (is (cffi:pointerp frustum))))))

(test graphene-with-frustum.3
  (let ((left 0) (right 1) (bottom 0) (top 1) (znear 1) (zfar 2))
    (graphene:with-matrix (matrix)
      (graphene:matrix-init-frustum matrix left right bottom top znear zfar)
      (graphene:with-frustums ((frustum (matrix graphene:matrix-t))
                               (frustum1 frustum))
        (is (graphene:frustum-equal frustum frustum1))))))

(test graphene-with-frustum.4
  (let ((left 0) (right 1) (bottom 0) (top 1) (znear 1) (zfar 2))
    (graphene:with-matrix (matrix)
      (graphene:matrix-init-frustum matrix left right bottom top znear zfar)
      (graphene:with-frustum (frustum (matrix graphene:matrix-t))
        (graphene:with-planes (plane1 plane2 plane3 plane4 plane5 plane6)
          (let ((planes (graphene:frustum-planes frustum
                                                 (list plane1 plane2 plane3
                                                       plane4 plane5 plane6))))
            (graphene:with-frustum (frustum1 (first planes)
                                             (second planes)
                                             (third planes)
                                             (fourth planes)
                                             (fifth planes)
                                             (sixth planes))
            (is (graphene:frustum-equal frustum frustum1)))))))))

;;;     graphene_frustum_get_planes

(test graphene-frustum-planes
  (let ((left -1) (right 1) (bottom -1) (top 1) (znear 1) (zfar 10))
    (graphene:with-matrix (matrix)
      (graphene:matrix-init-frustum matrix left right bottom top znear zfar)
      (graphene:with-frustum (frustum (matrix graphene:matrix-t))
        (graphene:with-planes (plane1 plane2 plane3 plane4 plane5 plane6)
          (is (every #'cffi:pointer-eq
                      (list plane1 plane2 plane3 plane4 plane5 plane6)
                      (graphene:frustum-planes frustum
                                               (list plane1 plane2 plane3
                                                     plane4 plane5 plane6))))
          (graphene:with-vec3 (vec)
            (is (approx-equal 0.0 (graphene:plane-constant plane1)))
            (is (every #'approx-equal
                       (list (- (/ (sqrt 2))) 0.0 (- (/ (sqrt 2))))
                       (graphene:vec3-to-float
                           (graphene:plane-normal plane1 vec))))
            (is (approx-equal 0.0 (graphene:plane-constant plane2)))
            (is (every #'approx-equal
                       (list (/ (sqrt 2)) 0.0 (- (/ (sqrt 2))))
                       (graphene:vec3-to-float
                           (graphene:plane-normal plane2 vec))))
            (is (approx-equal 0.0 (graphene:plane-constant plane3)))
            (is (every #'approx-equal
                       (list 0.0 (- (/ (sqrt 2))) (- (/ (sqrt 2))))
                       (graphene:vec3-to-float
                           (graphene:plane-normal plane3 vec))))
            (is (approx-equal 0.0 (graphene:plane-constant plane4)))
            (is (every #'approx-equal
                       (list 0.0 (/ (sqrt 2)) (- (/ (sqrt 2))))
                       (graphene:vec3-to-float
                           (graphene:plane-normal plane4 vec))))
            (is (approx-equal 10.0 (graphene:plane-constant plane5)))
            (is (every #'approx-equal
                       (list 0.0 0.0 1.0)
                       (graphene:vec3-to-float
                           (graphene:plane-normal plane5 vec))))
            (is (approx-equal -1.0 (graphene:plane-constant plane6)))
            (is (every #'approx-equal
                       (list 0.0 0.0 -1.0)
                       (graphene:vec3-to-float
                           (graphene:plane-normal plane6 vec))))))))))

;;;     graphene_frustum_contains_point

(test graphene-frustum-contains-point
  (let ((left -1) (right 1) (bottom -1) (top 1) (znear 1) (zfar 10))
    (graphene:with-matrix (matrix)
      (graphene:matrix-init-frustum matrix left right bottom top znear zfar)
      (graphene:with-frustum (frustum (matrix graphene:matrix-t))
        (graphene:with-point3d (point)
          (graphene:point3d-init point 0 0 0)
          (is-false (graphene:frustum-contains-point frustum point))
          (graphene:point3d-init point 0 0 -1)
          (is-true (graphene:frustum-contains-point frustum point))
          (graphene:point3d-init point 0 0 -2)
          (is-true (graphene:frustum-contains-point frustum point))
          (graphene:point3d-init point 0 0 -10)
          (is-true (graphene:frustum-contains-point frustum point))
          (graphene:point3d-init point 0 0 -11)
          (is-false (graphene:frustum-contains-point frustum point)))))))

;;;     graphene_frustum_intersects_sphere

(test graphene-frustum-intersects-sphere
  (let ((left -1) (right 1) (bottom -1) (top 1) (znear 1) (zfar 10))
    (graphene:with-matrix (matrix)
      (graphene:matrix-init-frustum matrix left right bottom top znear zfar)
      (graphene:with-frustum (frustum (matrix graphene:matrix-t))
        (graphene:with-objects ((center graphene:point3d-t)
                                (sphere graphene:sphere-t))
          (graphene:point3d-init center 0 0 0)
          (graphene:sphere-init sphere center 0.5)
          (is-false (graphene:frustum-intersects-sphere frustum sphere))
          (graphene:point3d-init center 0 0 -1)
          (graphene:sphere-init sphere center 0.5)
          (is-true (graphene:frustum-intersects-sphere frustum sphere))
          (graphene:point3d-init center 0 0 -2)
          (graphene:sphere-init sphere center 0.5)
          (is-true (graphene:frustum-intersects-sphere frustum sphere))
          (graphene:point3d-init center 0 0 -10)
          (graphene:sphere-init sphere center 0.5)
          (is-true (graphene:frustum-intersects-sphere frustum sphere))
          (graphene:point3d-init center 0 0 -11)
          (graphene:sphere-init sphere center 0.5)
          (is-false (graphene:frustum-intersects-sphere frustum sphere)))))))

;;;     graphene_frustum_intersects_box

(test graphene-frustum-intersects-box
  (let ((left -1) (right 1) (bottom -1) (top 1) (znear 1) (zfar 10))
    (graphene:with-matrix (matrix)
      (graphene:matrix-init-frustum matrix left right bottom top znear zfar)
      (graphene:with-frustum (frustum (matrix graphene:matrix-t))
        (graphene:with-objects ((pmin graphene:point3d-t)
                                (pmax graphene:point3d-t)
                                (box graphene:box-t))
          (graphene:point3d-init pmin 0 0 0)
          (graphene:point3d-init pmax 0.5 0.5 -0.5)
          (graphene:box-init box pmin pmax)
          (is-false (graphene:frustum-intersects-box frustum box))
          (graphene:point3d-init pmin 0 0 -1)
          (graphene:point3d-init pmax 0.5 0.5 -1.5)
          (graphene:box-init box pmin pmax)
          (is-true (graphene:frustum-intersects-box frustum box))
          (graphene:point3d-init pmin 0 0 -2)
          (graphene:point3d-init pmax 0.5 0.5 -2.5)
          (graphene:box-init box pmin pmax)
          (is-true (graphene:frustum-intersects-box frustum box))
          (graphene:point3d-init pmin 0 0 -10)
          (graphene:point3d-init pmax 0.5 0.5 -10.5)
          (graphene:box-init box pmin pmax)
          (is-true (graphene:frustum-intersects-box frustum box))
          (graphene:point3d-init pmin 0 0 -11)
          (graphene:point3d-init pmax 0.5 0.5 -11.5)
          (graphene:box-init box pmin pmax)
          (is-false (graphene:frustum-intersects-box frustum box)))))))

;;; 2024-12-26
