(in-package :graphene-test)

(def-suite graphene-matrix :in graphene-suite)
(in-suite graphene-matrix)

;;; --- Types and Values -------------------------------------------------------

;;;     graphene_matrix_t

;;; --- Macros -----------------------------------------------------------------

(test with-matrix.1
  (graphene:with-matrix (matrix)
    (is (graphene:matrix-is-identity matrix))))

(test with-matrix.2
  (graphene:with-matrices (matrix1 (matrix matrix1))
    (is (graphene:matrix-is-identity matrix))
    (is (graphene:matrix-is-identity matrix1))))

(test with-matrix.3
  (graphene:with-point3d (point 1 1 1)
    (graphene:with-matrix (matrix point)
      (is (cffi:pointerp matrix))
)))

(test with-matrix.4
  (graphene:with-vec3 (vector 1 1 1)
    (graphene:with-matrix (matrix 1.0 (vector graphene:vec3-t))
      (is (cffi:pointerp matrix))
)))

(test with-matrix.5
  (graphene:with-matrix (matrix 1.0 2.0)
    (is (cffi:pointerp matrix))
))

(test with-matrix.6
  (graphene:with-matrix (matrix 1.0 2.0 3.0)
    (is (cffi:pointerp matrix))
))

(test with-matrix.7
  (graphene:with-vec3s ((v1 1 0 0) (v2 0 1 0) (v3 0 0 1))
    (graphene:with-matrix (matrix (v1 graphene:vec3-t) v2 v3)
      (is (cffi:pointerp matrix))
)))

(test with-matrix.8
  (graphene:with-matrix (matrix  1 2 3 4)
    (is (cffi:pointerp matrix))
))

(test with-matrix.9
  (graphene:with-matrix (matrix 1 2 3 4 5 6)
    (is (cffi:pointerp matrix))
))

(test with-matrix.10
  (graphene:with-matrix (matrix (1 :double) 2 3 4 5 6)
    (is (cffi:pointerp matrix))
))

(test with-matrix.11
  (graphene:with-matrix (matrix 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)
    (is (cffi:pointerp matrix))
))

;;; --- Functions --------------------------------------------------------------

;;;     graphene_matrix_alloc
;;;     graphene_matrix_free

(test matrix-alloc
  (let (matrix)
    (is (cffi:pointerp (setf matrix (graphene:matrix-alloc))))
    (is-false (graphene:matrix-free matrix))))

;;;     graphene_matrix_init_identity

#+nil
(test matrix-init-identity
  (graphene:with-matrix (matrix)
    (is (cffi:pointer-eq matrix
                    (graphene:matrix-init-identity matrix)))
    (is (graphene:matrix-is-identity matrix))))

;;;     graphene_matrix_init_from_float

#+nil
(test matrix-init-from-float.1
  (graphene:with-matrix (matrix  1.0  2.0  3.0  4.0
                                 5.0  6.0  7.0  8.0
                                 9.0 10.0 11.0 12.0
                                13.0 14.0 15.0 16.0)
    (is (equal '(1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 10.0 11.0
                 12.0 13.0 14.0 15.0 16.0)
               (graphene:matrix-to-float matrix)))))

#+nil
(test matrix-init-from-float.2
  (graphene:with-matrix (matrix)
    ;; More than 16 values are ignored.
    (let ((values '(1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 10.0 11.0 12.0 13.0
                    14.0 15.0 16.0 17.0 18.0)))
      (is (cffi:pointerp (setf matrix
                          (graphene:matrix-init-from-float matrix values))))
      (is (equal '(1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 10.0 11.0 12.0 13.0
                    14.0 15.0 16.0)
                 (graphene:matrix-to-float matrix))))))

#+nil
(test matrix-init-from-float.3
  (graphene:with-matrix (matrix)
    ;; The list of values does not contain 16 values.
    (let ((values '(1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 10.0 11.0 12.0 13.0)))
      (signals (error)
               (graphene:matrix-init-from-float matrix values)))))

;;;     graphene_matrix_init_from_vec4

;;;     graphene_matrix_init_from_matrix

#+nil
(test matrix-init-from-matrix
  (graphene:with-matrix (matrix1)
    (graphene:with-matrix (matrix2)
      (is (graphene:matrix-is-identity
            (setf matrix1 (graphene:matrix-init-identity matrix1))))
      (is (graphene:matrix-is-identity
            (setf matrix2
                  (graphene:matrix-init-from-matrix matrix2 matrix1)))))))

;;;     graphene_matrix_init_from_2d
;;;     graphene_matrix_init_perspective
;;;     graphene_matrix_init_ortho
;;;     graphene_matrix_init_look_at
;;;     graphene_matrix_init_frustum
;;;     graphene_matrix_init_scale
;;;     graphene_matrix_init_translate
;;;     graphene_matrix_init_rotate
;;;     graphene_matrix_init_skew

;;;     graphene_matrix_is_identity

#+nil
(test matrix-is-identity
  (let ((matrix (graphene:matrix-alloc)))
    (is (cffi:pointerp (setf matrix (graphene:matrix-init-identity matrix))))
    (is (graphene:matrix-is-identity matrix))
    (is-false (graphene:matrix-free matrix))))

;;;     graphene_matrix_is_2d
;;;     graphene_matrix_is_backface_visible
;;;     graphene_matrix_is_singular

;;;     graphene_matrix_to_float

#+nil
(test matrix-to-float
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

;;; 2023-12-2
