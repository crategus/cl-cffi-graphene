(in-package :graphene-test)

(def-suite graphene-matrix :in graphene-suite)
(in-suite graphene-matrix)

;;; --- Types and Values -------------------------------------------------------

;;;     graphene_matrix_t

;;; --- Macros -----------------------------------------------------------------

;; No arguments => matrix-init-identity
(test graphene-with-matrix-macro.1
  (is (equal '(LET ((MATRIX (GRAPHENE:MATRIX-ALLOC)))
                (GRAPHENE:MATRIX-INIT-IDENTITY MATRIX)
                (UNWIND-PROTECT
                  (PROGN MATRIX)
                  (GRAPHENE:MATRIX-FREE MATRIX)))
             (macroexpand '(graphene:with-matrix (matrix) matrix)))))

;; One argument, no type => matrix-init-from-matrix
(test graphene-with-matrix-macro.2
  (is (equal '(LET ((MATRIX (GRAPHENE:MATRIX-ALLOC)))
                (GRAPHENE:MATRIX-INIT-FROM-MATRIX MATRIX MATRIX1)
                (UNWIND-PROTECT
                  (PROGN MATRIX)
                  (GRAPHENE:MATRIX-FREE MATRIX)))
             (macroexpand '(graphene:with-matrix (matrix matrix1) matrix)))))

;; One expression => matrix-init-from-matrix
(test graphene-with-matrix-macro.3
  (is (equal '(LET ((MATRIX (GRAPHENE:MATRIX-ALLOC)))
                (GRAPHENE:MATRIX-INIT-FROM-MATRIX MATRIX (FUNC X Y Z))
                (UNWIND-PROTECT
                  (PROGN MATRIX)
                  (GRAPHENE:MATRIX-FREE MATRIX)))
             (macroexpand '(graphene:with-matrix
                             (matrix (func x y z)) matrix)))))

;; One argument of type matrix-t => matrix-init-from-matrix
(test graphene-with-matrix-macro.4
  (is (equal '(LET ((MATRIX (GRAPHENE:MATRIX-ALLOC)))
                (GRAPHENE:MATRIX-INIT-FROM-MATRIX MATRIX MATRIX1)
                (UNWIND-PROTECT
                  (PROGN MATRIX)
                  (GRAPHENE:MATRIX-FREE MATRIX)))
             (macroexpand
               '(graphene:with-matrix (matrix (matrix1 graphene:matrix-t))
                   matrix)))))

;; One expression of type matrix-t => matrix-init-from-matrix
(test graphene-with-matrix-macro.5
  (is (equal '(LET ((MATRIX (GRAPHENE:MATRIX-ALLOC)))
                (GRAPHENE:MATRIX-INIT-FROM-MATRIX MATRIX (FUNC X Y Z))
                (UNWIND-PROTECT
                  (PROGN MATRIX)
                  (GRAPHENE:MATRIX-FREE MATRIX)))
             (macroexpand
               '(graphene:with-matrix (matrix ((func x y z) graphene:matrix-t))
                   matrix)))))

;; One argument of type point3d-t => matrix-init-translate
(test graphene-with-matrix-macro.6
  (is (equal '(LET ((MATRIX (GRAPHENE:MATRIX-ALLOC)))
                (GRAPHENE:MATRIX-INIT-TRANSLATE MATRIX POINT)
                (UNWIND-PROTECT
                  (PROGN MATRIX)
                  (GRAPHENE:MATRIX-FREE MATRIX)))
             (macroexpand
               '(graphene:with-matrix (matrix (point graphene:point3d-t))
                   matrix)))))

;; One expression of type point3d-t => matrix-init-translate
(test graphene-with-matrix-macro.7
  (is (equal '(LET ((MATRIX (GRAPHENE:MATRIX-ALLOC)))
                (GRAPHENE:MATRIX-INIT-TRANSLATE MATRIX (FUNC X Z Y))
                (UNWIND-PROTECT
                  (PROGN MATRIX)
                  (GRAPHENE:MATRIX-FREE MATRIX)))
             (macroexpand
               '(graphene:with-matrix (matrix ((func x z y) graphene:point3d-t))
                   matrix)))))

;; Two arguments of type float => matrix-init-skew
(test graphene-with-matrix-macro.8
  (is (equal '(LET ((MATRIX (GRAPHENE:MATRIX-ALLOC)))
                (GRAPHENE:MATRIX-INIT-SKEW MATRIX X Y)
                (UNWIND-PROTECT
                  (PROGN MATRIX)
                  (GRAPHENE:MATRIX-FREE MATRIX)))
             (macroexpand
               '(graphene:with-matrix (matrix x y) matrix)))))

;; Two expressions of type float => matrix-init-skew
(test graphene-with-matrix-macro.9
  (is (equal '(LET ((MATRIX (GRAPHENE:MATRIX-ALLOC)))
                (GRAPHENE:MATRIX-INIT-SKEW MATRIX (FUNC X Y) (FUNC A B))
                (UNWIND-PROTECT
                  (PROGN MATRIX)
                  (GRAPHENE:MATRIX-FREE MATRIX)))
             (macroexpand
               '(graphene:with-matrix (matrix (func x y) (func a b)) matrix)))))

;; Two arguments, second of type vec3-t => matrix-init-rotate
(test graphene-with-matrix-macro.10
  (is (equal '(LET ((MATRIX (GRAPHENE:MATRIX-ALLOC)))
                (GRAPHENE:MATRIX-INIT-ROTATE MATRIX A V)
                (UNWIND-PROTECT
                  (PROGN MATRIX)
                  (GRAPHENE:MATRIX-FREE MATRIX)))
             (macroexpand
               '(graphene:with-matrix (matrix a (v graphene:vec3-t)) matrix)))))

;; Two expressions, second of type vec3-t => matrix-init-rotate
(test graphene-with-matrix-macro.10
  (is (equal '(LET ((MATRIX (GRAPHENE:MATRIX-ALLOC)))
                (GRAPHENE:MATRIX-INIT-ROTATE MATRIX (FUNC A B) (FUNC X Y))
                (UNWIND-PROTECT
                  (PROGN MATRIX)
                  (GRAPHENE:MATRIX-FREE MATRIX)))
             (macroexpand
               '(graphene:with-matrix (matrix (func a b)
                                              ((func x y) graphene:vec3-t))
                  matrix)))))

;; Three arguments of type float => matrix-init-scale
(test graphene-with-matrix-macro.11
  (is (equal '(LET ((MATRIX (GRAPHENE:MATRIX-ALLOC)))
                (GRAPHENE:MATRIX-INIT-SCALE MATRIX X Y Z)
                (UNWIND-PROTECT
                  (PROGN MATRIX)
                  (GRAPHENE:MATRIX-FREE MATRIX)))
             (macroexpand
               '(graphene:with-matrix (matrix x y z) matrix)))))

;; Three expressions of type float => matrix-init-scale
(test graphene-with-matrix-macro.12
  (is (equal '(LET ((MATRIX (GRAPHENE:MATRIX-ALLOC)))
                (GRAPHENE:MATRIX-INIT-SCALE MATRIX
                                            (FUNC X Y) (FUNC A B) (FUNC C D))
                  (UNWIND-PROTECT
                    (PROGN MATRIX)
                    (GRAPHENE:MATRIX-FREE MATRIX)))
             (macroexpand
               '(graphene:with-matrix (matrix (func x y) (func a b) (func c d))
                  matrix)))))

;; Three arguments, first of type vec3-t => matrix-init-look-at
(test graphene-with-matrix-macro.13
  (is (equal '(LET ((MATRIX (GRAPHENE:MATRIX-ALLOC)))
                (GRAPHENE:MATRIX-INIT-LOOK-AT MATRIX X Y Z)
                (UNWIND-PROTECT
                  (PROGN MATRIX)
                  (GRAPHENE:MATRIX-FREE MATRIX)))
             (macroexpand
               '(graphene:with-matrix (matrix (x graphene:vec3-t) y z)
                  matrix)))))

;; Three expressions, first of type vec3-t => matrix-init-look-at
(test graphene-with-matrix-macro.14
  (is (equal '(LET ((MATRIX (GRAPHENE:MATRIX-ALLOC)))
                (GRAPHENE:MATRIX-INIT-LOOK-AT MATRIX
                                              (FUNC X Y) (FUNC A B) (FUNC C D))
                (UNWIND-PROTECT
                  (PROGN MATRIX)
                  (GRAPHENE:MATRIX-FREE MATRIX)))
             (macroexpand
               '(graphene:with-matrix (matrix ((func x y) graphene:vec3-t)
                                              (func a b) (func c d))
                  matrix)))))

;; Four arguments of type float => matrix-init-perspective
(test graphene-with-matrix-macro.15
  (is (equal '(LET ((MATRIX (GRAPHENE:MATRIX-ALLOC)))
                (GRAPHENE:MATRIX-INIT-PERSPECTIVE MATRIX X Y Z W)
                (UNWIND-PROTECT
                  (PROGN MATRIX)
                  (GRAPHENE:MATRIX-FREE MATRIX)))
             (macroexpand
               '(graphene:with-matrix (matrix x y z w)
                  matrix)))))

;; Four expressions of type float => matrix-init-perspective
(test graphene-with-matrix-macro.16
  (is (equal '(LET ((MATRIX (GRAPHENE:MATRIX-ALLOC)))
                (GRAPHENE:MATRIX-INIT-PERSPECTIVE MATRIX
                                                  (FUNC X A) (FUNC Y B)
                                                  (FUNC Z C) (FUNC W D))
                (UNWIND-PROTECT
                  (PROGN MATRIX)
                  (GRAPHENE:MATRIX-FREE MATRIX)))
             (macroexpand
               '(graphene:with-matrix (matrix (func x a) (func y b)
                                              (func z c) (func w d))
                  matrix)))))

;; Six arguments of type single float => matrix-init-ortho
(test graphene-with-matrix-macro.17
  (is (equal '(LET ((MATRIX (GRAPHENE:MATRIX-ALLOC)))
                (GRAPHENE:MATRIX-INIT-ORTHO MATRIX A B C D E F)
                (UNWIND-PROTECT
                  (PROGN MATRIX)
                  (GRAPHENE:MATRIX-FREE MATRIX)))
             (macroexpand
               '(graphene:with-matrix (matrix a b c d e f)
                  matrix)))))

;; Six expressions of type single float => matrix-init-ortho
(test graphene-with-matrix-macro.18
  (is (equal '(LET ((MATRIX (GRAPHENE:MATRIX-ALLOC)))
                (GRAPHENE:MATRIX-INIT-ORTHO MATRIX
                                            (FUNC A A1) (FUNC B B1) (FUNC C C1)
                                            (FUNC D D1) (FUNC E E1) (FUNC F F1))
                  (UNWIND-PROTECT
                    (PROGN MATRIX)
                    (GRAPHENE:MATRIX-FREE MATRIX)))
             (macroexpand
               '(graphene:with-matrix (matrix (func a a1)
                                              (func b b1)
                                              (func c c1)
                                              (func d d1)
                                              (func e e1) (func f f1))
                  matrix)))))

;; Six arguments, first of type double float => matrix-init-from-2d
(test graphene-with-matrix-macro.19
  (is (equal '(LET ((MATRIX (GRAPHENE:MATRIX-ALLOC)))
                (GRAPHENE:MATRIX-INIT-FROM-2D MATRIX A B C D E F)
                (UNWIND-PROTECT
                  (PROGN MATRIX)
                  (GRAPHENE:MATRIX-FREE MATRIX)))
             (macroexpand
               '(graphene:with-matrix (matrix (a :double) b c d e f)
                  matrix)))))

;; Six expressions, first of type double float => matrix-init-from-2d
(test graphene-with-matrix-macro.20
  (is (equal '(LET ((MATRIX (GRAPHENE:MATRIX-ALLOC)))
                (GRAPHENE:MATRIX-INIT-FROM-2D MATRIX
                                              (FUNC A A1) (FUNC B B1)
                                              (FUNC C C1) (FUNC D D1)
                                              (FUNC E E1) (FUNC F F1))
                  (UNWIND-PROTECT
                    (PROGN MATRIX)
                    (GRAPHENE:MATRIX-FREE MATRIX)))
             (macroexpand
               '(graphene:with-matrix (matrix ((func a a1) :double)
                                              (func b b1)
                                              (func c c1)
                                              (func d d1)
                                              (func e e1)
                                              (func f f1))
                  matrix)))))

;; Sixteen arguments => matrix-init-from-float
(test graphene-with-matrix-macro.21
  (is (equal '(LET ((MATRIX (GRAPHENE:MATRIX-ALLOC)))
                (GRAPHENE:MATRIX-INIT-FROM-FLOAT MATRIX
                                                 A B C D
                                                 E F G H
                                                 I J K L
                                                 M N O P)
                  (UNWIND-PROTECT
                    (PROGN MATRIX)
                    (GRAPHENE:MATRIX-FREE MATRIX)))
             (macroexpand
               '(graphene:with-matrix (matrix a b c d e f g h i j k l m n o p)
                  matrix)))))

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
      (is (cffi:pointerp matrix)))))

(test graphene-with-matrix.4
  (graphene:with-vec3 (vector 1 1 1)
    (graphene:with-matrix (matrix 1.0 (vector graphene:vec3-t))
      (is (cffi:pointerp matrix)))))

(test graphene-with-matrix.5
  (graphene:with-matrix (matrix 1.0 2.0)
    (is (cffi:pointerp matrix))))

(test graphene-with-matrix.6
  (graphene:with-matrix (matrix 1.0 2.0 3.0)
    (is (cffi:pointerp matrix))))

(test graphene-with-matrix.7
  (graphene:with-vec3s ((v1 1 0 0) (v2 0 1 0) (v3 0 0 1))
    (graphene:with-matrix (matrix (v1 graphene:vec3-t) v2 v3)
      (is (cffi:pointerp matrix)))))

(test graphene-with-matrix.8
  (graphene:with-matrix (matrix  1 2 3 4)
    (is (cffi:pointerp matrix))))

(test graphene-with-matrix.9
  (graphene:with-matrix (matrix 1 2 3 4 5 6)
    (is (cffi:pointerp matrix))))

(test graphene-with-matrix.10
  (graphene:with-matrix (matrix (1 :double) 2 3 4 5 6)
    (is (cffi:pointerp matrix))))

(test graphene-with-matrix.11
  (graphene:with-matrix (matrix 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)
    (is (cffi:pointerp matrix))))

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
      (is (graphene:matrix-near matrix matrix1 1.0e-6))))))

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

(test graphene-matrix-is-identity
  (graphene:with-matrix (matrix)
    (is (graphene:matrix-is-identity matrix))))

;;;     graphene_matrix_is_2d

(test graphene-matrix-is-2d
  (graphene:with-matrix (matrix (1 :double) 2 3 4 5 6)
    (is (equal '(1.0 2.0 0.0 0.0
                 3.0 4.0 0.0 0.0
                 0.0 0.0 1.0 0.0
                 5.0 6.0 0.0 1.0)
               (graphene:matrix-to-float matrix)))
    (is-true (graphene:matrix-is-2d matrix))))

;;;     graphene_matrix_is_backface_visible

(test graphene-matrix-is-backface-visible
  (graphene:with-matrix (matrix)
    (is-false (graphene:matrix-is-backface-visible matrix))))

;;;     graphene_matrix_is_singular

(test graphene-matrix-is-singular
  (graphene:with-matrix (matrix)
    (is-false (graphene:matrix-is-singular matrix))))

;;;     graphene_matrix_to_float

(test graphene-matrix-to-float
  (graphene:with-matrix (matrix)
    (is (cffi:pointerp (setf matrix (graphene:matrix-init-identity matrix))))
    (is (equal '(1.0 0.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 0.0 1.0)
               (graphene:matrix-to-float matrix)))))

;;;     graphene_matrix_to_2d

(test graphene-matrix-to-2d
  (graphene:with-matrix (matrix (1 :double) 2 3 4 5 6)
    (is (equal '(1.0d0 2.0d0 3.0d0 4.0d0 5.0d0 6.0d0)
               (graphene:matrix-to-2d matrix)))))

;;;     graphene_matrix_get_row

(test graphene-matrix-row
  (graphene:with-vec4 (result)
    (graphene:with-matrix (matrix (1 :double) 2 3 4 5 6)
      (is (equal '(1.0 2.0 0.0 0.0)
                 (graphene:vec4-to-float
                     (graphene:matrix-row matrix 0 result))))
      (is (equal '(3.0 4.0 0.0 0.0)
                 (graphene:vec4-to-float
                     (graphene:matrix-row matrix 1 result))))
      (is (equal '(0.0 0.0 1.0 0.0)
                 (graphene:vec4-to-float
                     (graphene:matrix-row matrix 2 result))))
      (is (equal '(5.0 6.0 0.0 1.0)
                 (graphene:vec4-to-float
                     (graphene:matrix-row matrix 3 result)))))))

;;;     graphene_matrix_get_value

(test graphene-matrix-value
  (graphene:with-matrix (matrix (1 :double) 2 3 4 5 6)
    ;; Values from first row
    (is (= 1.0 (graphene:matrix-value matrix 0 0)))
    (is (= 2.0 (graphene:matrix-value matrix 0 1)))
    (is (= 0.0 (graphene:matrix-value matrix 0 2)))
    (is (= 0.0 (graphene:matrix-value matrix 0 3)))
    ;; Values from second row
    (is (= 3.0 (graphene:matrix-value matrix 1 0)))
    (is (= 4.0 (graphene:matrix-value matrix 1 1)))
    (is (= 0.0 (graphene:matrix-value matrix 1 2)))
    (is (= 0.0 (graphene:matrix-value matrix 1 3)))))

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

;;; 2025-4-6
