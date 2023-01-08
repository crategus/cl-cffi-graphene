(in-package :graphene-test)

(def-suite graphene-matrix :in graphene-suite)
(in-suite graphene-matrix)

;;; --- Types and Values -------------------------------------------------------

;;;     graphene_matrix_t

;;; --- Macros -----------------------------------------------------------------

(test with-graphene-matrix.1
  (with-graphene-matrix (matrix)
    (is (matrix-is-identity matrix))))

(test with-graphene-matrix.2
  (with-graphene-matrices (matrix1 (matrix matrix1))
    (is (matrix-is-identity matrix))
    (is (matrix-is-identity matrix1))))

(test with-graphene-matrix.3
  (with-graphene-point3d (point 1 1 1)
    (with-graphene-matrix (matrix point)
      (is (pointerp matrix))
)))

(test with-graphene-matrix.4
  (with-graphene-vec3 (vector 1 1 1)
    (with-graphene-matrix (matrix 1.0 (vector vec3-t))
      (is (pointerp matrix))
)))

(test with-graphene-matrix.5
  (with-graphene-matrix (matrix 1.0 2.0)
    (is (pointerp matrix))
))

(test with-graphene-matrix.6
  (with-graphene-matrix (matrix 1.0 2.0 3.0)
    (is (pointerp matrix))
))

(test with-graphene-matrix.7
  (with-graphene-vec3s ((v1 1 0 0) (v2 0 1 0) (v3 0 0 1))
    (with-graphene-matrix (matrix (v1 vec3-t) v2 v3)
      (is (pointerp matrix))
)))

(test with-graphene-matrix.8
  (with-graphene-matrix (matrix  1 2 3 4)
    (is (pointerp matrix))
))

(test with-graphene-matrix.9
  (with-graphene-matrix (matrix 1 2 3 4 5 6)
    (is (pointerp matrix))
))

(test with-graphene-matrix.10
  (with-graphene-matrix (matrix (1 :double) 2 3 4 5 6)
    (is (pointerp matrix))
))

(test with-graphene-matrix.11
  (with-graphene-matrix (matrix 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)
    (is (pointerp matrix))
))

;;; --- Functions --------------------------------------------------------------

;;;     graphene_matrix_alloc
;;;     graphene_matrix_free

(test matrix-alloc
  (let (matrix)
    (is (pointerp (setf matrix (matrix-alloc))))
    (is-false (matrix-free matrix))))

;;;     graphene_matrix_init_identity

#+nil
(test matrix-init-identity
  (with-graphene-matrix (matrix)
    (is (pointer-eq matrix
                    (matrix-init-identity matrix)))
    (is (matrix-is-identity matrix))))

;;;     graphene_matrix_init_from_float

#+nil
(test matrix-init-from-float.1
  (with-graphene-matrix (matrix  1.0  2.0  3.0  4.0
                                 5.0  6.0  7.0  8.0
                                 9.0 10.0 11.0 12.0
                                13.0 14.0 15.0 16.0)
    (is (equal '(1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 10.0 11.0
                 12.0 13.0 14.0 15.0 16.0)
               (matrix-to-float matrix)))))

#+nil
(test matrix-init-from-float.2
  (with-graphene-matrix (matrix)
    ;; More than 16 values are ignored.
    (let ((values '(1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 10.0 11.0 12.0 13.0
                    14.0 15.0 16.0 17.0 18.0)))
      (is (pointerp (setf matrix
                          (matrix-init-from-float matrix values))))
      (is (equal '(1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 10.0 11.0 12.0 13.0
                    14.0 15.0 16.0)
                 (matrix-to-float matrix))))))

#+nil
(test matrix-init-from-float.3
  (with-graphene-matrix (matrix)
    ;; The list of values does not contain 16 values.
    (let ((values '(1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 10.0 11.0 12.0 13.0)))
      (signals (error)
               (matrix-init-from-float matrix values)))))

;;;     graphene_matrix_init_from_vec4

;;;     graphene_matrix_init_from_matrix

#+nil
(test matrix-init-from-matrix
  (with-graphene-matrix (matrix1)
    (with-graphene-matrix (matrix2)
      (is (matrix-is-identity
            (setf matrix1 (matrix-init-identity matrix1))))
      (is (matrix-is-identity
            (setf matrix2
                  (matrix-init-from-matrix matrix2 matrix1)))))))

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
  (let ((matrix (matrix-alloc)))
    (is (pointerp (setf matrix (matrix-init-identity matrix))))
    (is (matrix-is-identity matrix))
    (is-false (matrix-free matrix))))

;;;     graphene_matrix_is_2d
;;;     graphene_matrix_is_backface_visible
;;;     graphene_matrix_is_singular

;;;     graphene_matrix_to_float

#+nil
(test matrix-to-float
  (with-graphene-matrix (matrix)
    (is (pointerp (setf matrix (matrix-init-identity matrix))))
    (is (equal '(1.0 0.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 0.0 1.0)
               (matrix-to-float matrix)))))

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

;;; 2022-9-14
