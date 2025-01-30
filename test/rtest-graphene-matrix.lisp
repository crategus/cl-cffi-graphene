(in-package :graphene-test)

(def-suite graphene-matrix :in graphene-suite)
(in-suite graphene-matrix)

;;; --- Types and Values -------------------------------------------------------

;;;     graphene_matrix_t

;;; --- Macros -----------------------------------------------------------------

(test graphene-with-matrix.1
  (graphene:with-matrix (matrix)
    (is (graphene:matrix-is-identity matrix))))

(test graphene-with-matrix.2
  (graphene:with-matrices (matrix1 (matrix matrix1))
    (is (graphene:matrix-is-identity matrix))
    (is (graphene:matrix-is-identity matrix1))))

(test graphene-with-matrix.3
  (graphene:with-point3d (point 1 1 1)
    (graphene:with-matrix (matrix point)
      (is (cffi:pointerp matrix))
)))

(test graphene-with-matrix.4
  (graphene:with-vec3 (vector 1 1 1)
    (graphene:with-matrix (matrix 1.0 (vector graphene:vec3-t))
      (is (cffi:pointerp matrix))
)))

(test graphene-with-matrix.5
  (graphene:with-matrix (matrix 1.0 2.0)
    (is (cffi:pointerp matrix))
))

(test graphene-with-matrix.6
  (graphene:with-matrix (matrix 1.0 2.0 3.0)
    (is (cffi:pointerp matrix))
))

(test graphene-with-matrix.7
  (graphene:with-vec3s ((v1 1 0 0) (v2 0 1 0) (v3 0 0 1))
    (graphene:with-matrix (matrix (v1 graphene:vec3-t) v2 v3)
      (is (cffi:pointerp matrix))
)))

(test graphene-with-matrix.8
  (graphene:with-matrix (matrix  1 2 3 4)
    (is (cffi:pointerp matrix))
))

(test graphene-with-matrix.9
  (graphene:with-matrix (matrix 1 2 3 4 5 6)
    (is (cffi:pointerp matrix))
))

(test graphene-with-matrix.10
  (graphene:with-matrix (matrix (1 :double) 2 3 4 5 6)
    (is (cffi:pointerp matrix))
))

(test graphene-with-matrix.11
  (graphene:with-matrix (matrix 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)
    (is (cffi:pointerp matrix))
))

;;; --- Functions --------------------------------------------------------------

;;;     graphene_matrix_alloc
;;;     graphene_matrix_free

(test graphene-matrix-alloc
  (let (matrix)
    (is (cffi:pointerp (setf matrix (graphene:matrix-alloc))))
    (is-false (graphene:matrix-free matrix))))

;;;     graphene_matrix_init_identity

(test graphene-matrix-init-identity
  (graphene:with-matrix (matrix)
    (is (cffi:pointer-eq matrix
                         (graphene:matrix-init-identity matrix)))
    (is (graphene:matrix-is-identity matrix))))

;;;     graphene_matrix_init_from_float

(test graphene-matrix-init-from-float
  (graphene:with-matrix (matrix  1.0  2.0  3.0  4.0
                                 5.0  6.0  7.0  8.0
                                 9.0 10.0 11.0 12.0
                                13.0 14.0 15.0 16.0)
    (is (equal '( 1.0  2.0  3.0  4.0
                  5.0  6.0  7.0  8.0
                  9.0 10.0 11.0 12.0
                 13.0 14.0 15.0 16.0)
               (graphene:matrix-to-float matrix)))))

;;;     graphene_matrix_init_from_vec4

(test graphene-matrix-init-form-vec4
  (graphene:with-vec4s ((v0 1 0 0 0)
                        (v1 0 1 0 0)
                        (v2 0 0 1 0)
                        (v3 0 0 0 1))
     (graphene:with-matrix (matrix)
       (is (cffi:pointer-eq matrix
                            (graphene:matrix-init-from-vec4 matrix
                                                            v0 v1 v2 v3)))
       (is (equal '(1.0 0.0 0.0 0.0
                    0.0 1.0 0.0 0.0
                    0.0 0.0 1.0 0.0
                    0.0 0.0 0.0 1.0)
                  (graphene:matrix-to-float matrix))))))

;;;     graphene_matrix_init_from_matrix

(test graphene-matrix-init-from-matrix
  (graphene:with-matrices (matrix1 matrix2)
    (is (graphene:matrix-is-identity
            (setf matrix1 (graphene:matrix-init-identity matrix1))))
    (is (graphene:matrix-is-identity
            (setf matrix2
                  (graphene:matrix-init-from-matrix matrix2 matrix1))))))

;;;     graphene_matrix_init_from_2d

(test graphene-matrix-init-from-2d
  (graphene:with-matrix (matrix)
    (is (cffi:pointer-eq matrix
                         (graphene:matrix-init-from-2d matrix 1 2 3 4 5 6)))
    (is (equal '(1.0 2.0 0.0 0.0
                 3.0 4.0 0.0 0.0
                 0.0 0.0 1.0 0.0
                 5.0 6.0 0.0 1.0)
               (graphene:matrix-to-float matrix)))))

;;;     graphene_matrix_init_perspective

(test graphene-matrix-init-perspective
  (let ((fovy 60) (aspect 2) (znear 1) (zfar 10))
    (graphene:with-matrices (matrix matrix1)

      (let* ((s (/ 1 (tan (/ (* fovy pi) 360))))
             (s1 (/ s aspect))
             (a (/ (+ zfar znear) (- znear zfar)))
             (b (/ (* 2 zfar znear) (- znear zfar))))
        (graphene:matrix-init-from-float matrix1
                s1  0  0   0
                0   s  0   0
                0   0  a  -1
                0   0  b   0)
          (is (equal '(0.8660254 0.0 0.0 0.0
                       0.0 1.7320508 0.0 0.0
                       0.0 0.0 -1.2222222 -1.0
                       0.0 0.0 -2.2222223 0.0)
                     (graphene:matrix-to-float matrix1)))

      (is (cffi:pointer-eq matrix
                           (graphene:matrix-init-perspective matrix
                                                             fovy
                                                             aspect
                                                             znear
                                                             zfar)))
      (is (equal '(0.86602545 0.0 0.0 0.0
                   0.0 1.7320509 0.0 0.0
                   0.0 0.0 -1.2222222 -1.0
                   0.0 0.0 -2.2222223 0.0)
                 (graphene:matrix-to-float matrix)))
      (is (graphene:matrix-near matrix matrix1 1.0e-6))
))))

;;;     graphene_matrix_init_ortho
;;;     graphene_matrix_init_look_at

;;;     graphene_matrix_init_frustum

(test graphene-matrix-init-frustum
  (graphene:with-matrices (matrix1 matrix2)
    (let ((left 0) (right 1) (bottom 0) (top 1) (znear 1) (zfar 2))
      ;; Initialize MATRIX1 from the definition
      (is (cffi:pointer-eq matrix1
                           (graphene:matrix-init-from-float matrix1
                               ;; first row
                               (/ (* 2 znear) (- right left))
                               0
                               0
                               0
                               ;; second row
                               0
                               (/ (* 2 znear) (- top bottom))
                               0
                               0
                               ;; third row
                               (/ (+ right left) (- right left))
                               (/ (+ top bottom) (- top bottom))
                               (- (/ (+ zfar znear) (- zfar znear)))
                               -1
                               ;; fourth row
                               0
                               0
                               (- (/ (* 2 zfar znear) (- zfar znear)))
                               0)))
      (is (every #'approx-equal
                 '(2.0  0.0   0.0   0.0
                   0.0  2.0   0.0   0.0
                   1.0  1.0  -3.0  -1.0
                   0.0  0.0  -4.0   0.0) (graphene:matrix-to-float matrix1)))
      ;; Initalize MATRIX2 from the given frustum values
      (is (cffi:pointer-eq matrix2
                           (graphene:matrix-init-frustum matrix2
                                                         left
                                                         right
                                                         bottom
                                                         top
                                                         znear
                                                         zfar)))
      (is (every #'approx-equal
                 '(2.0  0.0   0.0   0.0
                   0.0  2.0   0.0   0.0
                   1.0  1.0  -3.0  -1.0
                   0.0  0.0  -4.0   0.0) (graphene:matrix-to-float matrix2)))
      ;; MATRIX1 and MATRIX2 are equal
      (is (graphene:matrix-equal matrix1 matrix2)))))

(test graphene-matrix-init-frustum.1
  (graphene:with-matrices (matrix1 matrix2)
    (let ((left -1) (right 1) (bottom -1) (top 1) (znear 1) (zfar 2))
      ;; Initialize MATRIX1 from the definition
      (is (cffi:pointer-eq matrix1
                           (graphene:matrix-init-from-float matrix1
                               ;; first row
                               (/ (* 2 znear) (- right left))
                               0
                               0
                               0
                               ;; second row
                               0
                               (/ (* 2 znear) (- top bottom))
                               0
                               0
                               ;; third row
                               (/ (+ right left) (- right left))
                               (/ (+ top bottom) (- top bottom))
                               (- (/ (+ zfar znear) (- zfar znear)))
                               -1
                               ;; fourth row
                               0
                               0
                               (- (/ (* 2 zfar znear) (- zfar znear)))
                               0)))
      (is (every #'approx-equal
                 '(1.0  0.0   0.0   0.0
                   0.0  1.0   0.0   0.0
                   0.0  0.0  -3.0  -1.0
                   0.0  0.0  -4.0   0.0) (graphene:matrix-to-float matrix1)))
      ;; Initalize MATRIX2 from the given frustum values
      (is (cffi:pointer-eq matrix2
                           (graphene:matrix-init-frustum matrix2
                                                         left
                                                         right
                                                         bottom
                                                         top
                                                         znear
                                                         zfar)))
      (is (every #'approx-equal
                 '(1.0  0.0   0.0   0.0
                   0.0  1.0   0.0   0.0
                   0.0  0.0  -3.0  -1.0
                   0.0  0.0  -4.0   0.0) (graphene:matrix-to-float matrix2)))
      ;; MATRIX1 and MATRIX2 are equal
      (is (graphene:matrix-equal matrix1 matrix2)))))

;;;     graphene_matrix_init_scale
;;;     graphene_matrix_init_translate
;;;     graphene_matrix_init_rotate
;;;     graphene_matrix_init_skew

;;;     graphene_matrix_is_identity

#+nil
(test graphene-matrix-is-identity
  (let ((matrix (graphene:matrix-alloc)))
    (is (cffi:pointerp (setf matrix (graphene:matrix-init-identity matrix))))
    (is (graphene:matrix-is-identity matrix))
    (is-false (graphene:matrix-free matrix))))

;;;     graphene_matrix_is_2d
;;;     graphene_matrix_is_backface_visible
;;;     graphene_matrix_is_singular

;;;     graphene_matrix_to_float

(test graphene-matrix-to-float
  (graphene:with-matrix (matrix)
    (is (cffi:pointerp (setf matrix (graphene:matrix-init-identity matrix))))
    (is (equal '(1.0 0.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 0.0 1.0)
               (graphene:matrix-to-float matrix)))))

;;;     graphene_matrix_to_2d
;;;     graphene_matrix_get_row
;;;     graphene_matrix_get_value
;;;     graphene_matrix_multiply
;;;     graphene_matrix_determinant
;;;     graphene_matrix_transform_vec4
;;;     graphene_matrix_transform_vec3
;;;     graphene_matrix_transform_point
;;;     graphene_matrix_transform_point3d
;;;     graphene_matrix_transform_rect
;;;     graphene_matrix_transform_bounds
;;;     graphene_matrix_transform_box
;;;     graphene_matrix_transform_sphere
;;;     graphene_matrix_transform_ray
;;;     graphene_matrix_project_point
;;;     graphene_matrix_project_rect_bounds
;;;     graphene_matrix_project_rect
;;;     graphene_matrix_untransform_point
;;;     graphene_matrix_untransform_bounds
;;;     graphene_matrix_unproject_point3d
;;;     graphene_matrix_translate
;;;     graphene_matrix_rotate
;;;     graphene_matrix_rotate_x
;;;     graphene_matrix_rotate_y
;;;     graphene_matrix_rotate_z
;;;     graphene_matrix_rotate_quaternion
;;;     graphene_matrix_rotate_euler
;;;     graphene_matrix_scale
;;;     graphene_matrix_skew_xy
;;;     graphene_matrix_skew_xz
;;;     graphene_matrix_skew_yz
;;;     graphene_matrix_transpose
;;;     graphene_matrix_inverse
;;;     graphene_matrix_perspective
;;;     graphene_matrix_normalize
;;;     graphene_matrix_get_x_translation
;;;     graphene_matrix_get_y_translation
;;;     graphene_matrix_get_z_translation
;;;     graphene_matrix_get_x_scale
;;;     graphene_matrix_get_y_scale
;;;     graphene_matrix_get_z_scale
;;;     graphene_matrix_decompose
;;;     graphene_matrix_interpolate
;;;     graphene_matrix_equal
;;;     graphene_matrix_equal_fast
;;;     graphene_matrix_near
;;;     graphene_matrix_print

;;; 2023-12-10
