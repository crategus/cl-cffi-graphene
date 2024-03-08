;;; ----------------------------------------------------------------------------
;;; graphene.matrix.lisp
;;;
;;; The documentation of this file is taken from the GRAPHENE Reference Manual
;;; and modified to document the Lisp binding to the Graphene library. See
;;; <https://ebassi.github.io/graphene/docs/>. The API documentation of the Lisp
;;; binding is available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 - 2023 Dieter Kaiser
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;; ----------------------------------------------------------------------------
;;;
;;; Matrix
;;;
;;;     4x4 matrices
;;;
;;; Types and Values
;;;
;;;     graphene_matrix_t
;;;
;;; Functions
;;;
;;;     graphene_matrix_alloc
;;;     graphene_matrix_free
;;;     graphene_matrix_init_identity
;;;     graphene_matrix_init_from_float
;;;     graphene_matrix_init_from_vec4
;;;     graphene_matrix_init_from_matrix
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
;;;     graphene_matrix_is_2d
;;;     graphene_matrix_is_backface_visible
;;;     graphene_matrix_is_singular
;;;     graphene_matrix_to_float
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
;;; ----------------------------------------------------------------------------

(in-package :graphene)

(defmacro with-matrix ((var &rest args) &body body)
 #+liber-documentation
 "@version{2024-1-20}
  @syntax{(graphene:with-matrix (m) body) => result}
  @syntax{(graphene:with-matrix (m m1) body) => result}
  @syntax{(graphene:with-matrix (m (p graphene:point3d-t)) body) => result}
  @syntax{(graphene:with-matrix (m angle (axis graphene:vec3-t)) body)
    => result}
  @syntax{(graphene:with-matrix (m xskew yskew) body) => result}
  @syntax{(graphene:with-matrix (m x y z) body) => result}
  @syntax{(graphene:with-matrix (m (eye graphene:vec3-t) center up) body)
    => result }
  @syntax{(graphene:with-matrix (m fovy aspect znear zfar) body) => result}
  @syntax{(graphene:with-matrix (m (v0 graphene:vec4-t) v1 v2 v3) body)
    => result}
  @syntax{(graphene:with-matrix (m left right top bottom znear zfar) body)
    => result}
  @syntax{(graphene:with-matrix (m (xx :double) yx xy yy x0 y0) body)
    => result}
  @syntax{(graphene:with-matrix (m &rest args) body) => result}
  @argument[m]{a @symbol{graphene:matrix-t} instance to create and initialize}
  @argument[m1]{a @symbol{graphene:matrix-t} instance to use for initialization}
  @argument[p]{a @symbol{graphene:point3d-t} instance}
  @argument[axis, eye, center, up]{a @symbol{graphene:vec3-t} instance}
  @argument[v0,v1,v2,v3]{a @symbol{graphene:vec4-t} instance}
  @argument[xx, yx, xy, yy, x0, y0]{a double float value}
  @argument[other values]{a float value}
  @begin{short}
    The @fun{graphene:with-matrix} macro allocates a new
    @symbol{graphene:matrix-t} instance, initializes the box with the given
    values and executes the body that uses the box.
  @end{short}
  After execution of the body the allocated memory for the matrix is released.

  The macro uses the following function for intialization of the matrix:
  @begin{itemize}
    @item{@fun{graphene:matrix-init-identiy}}
    @item{@fun{graphene:matrix-init-from-matrix}}
    @item{@fun{graphene:matrix-init-translate}}
    @item{@fun{graphene:matrix-init-rotate}}
    @item{@fun{graphene:matrix-init-skew}}
    @item{@fun{graphene:matrix-init-scale}}
    @item{@fun{graphene:matrix-init-look-at}}
    @item{@fun{graphene:matrix-init-perspective}}
    @item{@fun{graphene:matrix-init-from-vec4}}
    @item{@fun{graphene:matrix-init-ortho}}
    @item{@fun{graphene:matrix-init-from-2d}}
    @item{@fun{graphene:matrix-init-from-float}}
  @end{itemize}
  @begin[Note]{dictionary}
    The memory is allocated with the @fun{graphene:matrix-alloc} function and
    released with the @fun{graphene:matrix-free} function.
  @end{dictionary}
  @see-symbol{graphene:matrix-t}
  @see-macro{graphene:with-matrices}
  @see-function{graphene:matrix-alloc}
  @see-function{graphene:matrix-free}"
  (cond ((null args)
         ;; No arguments, the default is initialization with identity
         `(let ((,var (matrix-alloc)))
            (matrix-init-identity ,var)
            (unwind-protect
              (progn ,@body)
              (matrix-free ,var))))
        ((null (second args))
         ;; One argument, the argument must be of type matrix-t or point3d-t
         (destructuring-bind (arg &optional type1) (mklist (first args))
           (cond ((or (not type1)
                      (eq type1 'matrix-t))
                  ;; One argument with no type or of type matrix-t
                  `(let ((,var (matrix-alloc)))
                     (matrix-init-from-matrix ,var ,arg)
                     (unwind-protect
                       (progn ,@body)
                       (matrix-free ,var))))
                 ((eq type1 'point3d-t)
                  ;; One argument of type point3d-t
                  `(let ((,var (matrix-alloc)))
                     (matrix-init-translate ,var ,arg)
                     (unwind-protect
                       (progn ,@body)
                       (matrix-free ,var))))
                 (t
                  (error "Syntax error in GRAPHENE:WITH-MATRIX")))))
        ((null (third args))
         ;; Two arguments, the first can be of type point3d-t or vec3-t
         (destructuring-bind ((arg1 &optional type1)
                              (arg2 &optional type2))
             (list (mklist (first args)) (mklist (second args)))
           (cond ((and (or (not type1)
                           (eq type1 :float))
                       (eq type2 'vec3-t))
                  ;; First argument with no type or of type :float and
                  ;; second argument with type vec3-t
                  `(let ((,var (matrix-alloc)))
                     (matrix-init-rotate ,var ,arg1 ,arg2)
                     (unwind-protect
                       (progn ,@body)
                       (matrix-free ,var))))
                 ((and (or (not type1)
                           (eq type1 :float))
                       (or (not type2)
                           (eq type2 :float)))
                  ;; First argument with no type or of type :float and second
                  ;;;argument with no type or type :float
                  `(let ((,var (matrix-alloc)))
                     (matrix-init-skew ,var ,arg1 ,arg2)
                     (unwind-protect
                       (progn ,@body)
                       (matrix-free ,var))))
                 (t
                  (error "Syntax error in GRAPHENE:WITH-MATRIX")))))
        ((null (fourth args))
         ;; Three arguments, the first can be of type :float or vec3-t
         (destructuring-bind (arg &optional type1) (mklist (first args))
           (cond ((or (not type1)
                      (eq type1 :float))
                    ;; First argument with no type or of type :float.
                    `(let ((,var (matrix-alloc)))
                       (matrix-init-scale ,var ,arg ,@(rest args))
                       (unwind-protect
                         (progn ,@body)
                         (matrix-free ,var))))
                 ((eq type1 'vec3-t)
                  ;; First argument with type vec3-t
                  `(let ((,var (matrix-alloc)))
                     (matrix-init-look-at ,var,arg ,@(rest args))
                     (unwind-protect
                       (progn ,@body)
                       (matrix-free ,var))))
                 (t
                  (error "Syntax error in GRAPHENE:WITH-MATRIX")))))
        ((null (fifth args))
         ;; Four arguments, the first can be of type :float or vec4-t
         (destructuring-bind (arg &optional type1) (mklist (first args))
           (cond ((and (or (not type1)
                           (eq type1 :float)))
                    ;; First argument with no type or of type :float.
                    `(let ((,var (matrix-alloc)))
                       (matrix-init-perspective ,var ,arg ,@(rest args))
                       (unwind-protect
                         (progn ,@body)
                         (matrix-free ,var))))
                 ((eq type1 'vec4-t)
                  ;; First argument with type vec4-t
                  `(let ((,var (matrix-alloc)))
                     (matrix-init-from-vec4 ,var ,arg ,@(rest args))
                     (unwind-protect
                       (progn ,@body)
                       (matrix-free ,var))))
                   (t
                    (error "syntax error in GRAPHENE:WITH-MATRIX")))))
        ((null (seventh args))
         ;; Six arguments, the first can be of type :float or :double
         (destructuring-bind (arg &optional type1) (mklist (first args))
           (cond ((and (or (not type1)
                           (eq type1 :float)))
                    ;; First argument with no type or of type :float.
                    `(let ((,var (matrix-alloc)))
                       (matrix-init-ortho ,var ,arg ,@(rest args))
                       (unwind-protect
                         (progn ,@body)
                         (matrix-free ,var))))
                 ((eq type1 :double)
                  ;; First argument with type :double
                  `(let ((,var (matrix-alloc)))
                     (matrix-init-from-2d ,var ,arg ,@(rest args))
                     (unwind-protect
                       (progn ,@body)
                       (matrix-free ,var))))
                   (t
                    (error "Syntax error in GRAPHENE:WITH-MATRIX")))))
        ((null (nth 16 args))
         ;; Sixteen arguments of type float
         `(let ((,var (matrix-alloc)))
            (matrix-init-from-float ,var ,@args)
            (unwind-protect
              (progn ,@body)
              (matrix-free ,var))))
        (t
         (error "Syntax error in GRAPHENE:WITH-MATRIX"))))

(export 'with-matrix)

(defmacro with-matrices (vars &body body)
 #+liber-documentation
 "@version{2024-1-20}
  @syntax{(graphene:with-matrices (matrix1 ... matrixn) body) => result}
  @argument[matrix1 ... matrixn]{the newly created @symbol{graphene:matrix-t}
    instances}
  @argument[body]{a body that uses the bindings @arg{matrix1 ... matrixn}}
  @begin{short}
    The @fun{graphene:with-matrices} macro creates new variable bindings and
    executes the body that use these bindings.
  @end{short}
  The macro performs the bindings sequentially, like the @sym{let*} macro.

  Each box can be initialized with values using the syntax for the
  @fun{graphene:with-matrix} macro. See also the @fun{graphene:with-matrix}
  documentation.
  @see-symbol{graphene:matrix-t}
  @see-macro{graphene:with-matrix}"
  (if vars
      (let ((var (mklist (first vars))))
        `(with-matrix ,var
           (with-matrices ,(rest vars)
             ,@body)))
      `(progn ,@body)))

(export 'with-matrices)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_t
;;; ----------------------------------------------------------------------------

(cffi:defcstruct matrix-t)

#+liber-documentation
(setf (liber:alias-for-symbol 'matrix-t)
      "CStruct"
      (liber:symbol-documentation 'matrix-t)
 "@version{#2023-12-4}
  @begin{short}
    The @symbol{graphene:matrix-t} structure is a type that provides a 4x4
    square matrix, useful for representing 3D transformations.
  @end{short}
  The matrix is treated as row-major, i.e. it has four vectors @code{x},
  @code{y}, @code{z}, and @code{w} representing rows, and elements of each
  vector are a column:
  @begin{pre}
⎡ m.x ⎤      ⎛ x.x  x.y  x.z  x.w ⎞
⎜ m.y ⎟  =>  ⎜ y.x  y.y  y.z  y.w ⎟
⎜ m.z ⎟      ⎜ z.x  z.y  z.z  z.w ⎟
⎣ m.w ⎦      ⎝ w.x  w.y  w.z  w.w ⎠
  @end{pre}
  It is possible to easily convert a @symbol{graphene:matrix-t} instance to and
  from an array of floating point values that can be used with other libraries.

  The contents of a @symbol{graphene:matrix-t} instance are private, and direct
  access is not possible. You can modify and read the contents of a
  @symbol{graphene:matrix-t} instance only through the provided API.

  @subheading{Conventions}
  Graphene uses left-multiplication for all its operations on vectors and
  matrices. In other words, given a matrix @code{A} and a vector @code{b}, the
  result of a multiplication is going to be:
  @begin{pre}
res = b × A
  @end{pre}
  Multiplying two matrices, on the other hand, will use right-multiplication.
  Given two matrices @code{A} and @code{B}, the result of the multiplication is
  going to be
  @begin{pre}
res = A × B
  @end{pre}
  as the implementation will multiply each row vector of matrix @code{A} with
  the matrix @code{B} to obtain the new row vectors of the result matrix:
  @begin{pre}
res = ⎡ A.x × B ⎤
      ⎜ A.y × B ⎟
      ⎜ A.z × B ⎟
      ⎣ A.w × B ⎦
  @end{pre}
  For more information, see the @code{simd4x4f-t} documentation, especially
  the following functions: @code{simd4x4f-vec4-mul}, @code{simd4x4f-vec3-mul},
  @code{simd4x4f-point3-mul}, and @code{simd4x4f-matrix-mul}.")

(export 'matrix-t)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_alloc ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_matrix_alloc" matrix-alloc)
    (:pointer (:struct matrix-t))
 #+liber-documentation
 "@version{#2023-12-4}
  @return{The newly allocated @symbol{graphene:matrix-t} instance.}
  @short{Allocates a new @symbol{graphene:matrix-t} instance.}
  The contents of the returned structure are undefined.
  @see-symbol{graphene:matrix-t}
  @see-function{graphene:matrix-free}")

(export 'matrix-alloc)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_free ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_matrix_free" matrix-free) :void
 #+liber-documentation
 "@version{#2023-12-4}
  @argument[m]{a @symbol{graphene:matrix-t} instance}
  @begin{short}
    Frees the resources allocated by the @fun{graphene:matrix-alloc} function.
  @end{short}
  @see-symbol{graphene:matrix-t}
  @see-function{graphene:matrix-alloc}"
  (m (:pointer (:struct matrix-t))))

(export 'matrix-free)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_init_identity ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_matrix_init_identity" matrix-init-identity)
    (:pointer (:struct matrix-t))
 #+liber-documentation
 "@version{#2023-12-4}
  @argument[m]{a @symbol{graphene:matrix-t} instance}
  @return{The initialized @symbol{graphene:matrix-t} instance.}
  @short{Initializes the matrix with the identity matrix.}
  @see-symbol{graphene:matrix-t}"
  (m (:pointer (:struct matrix-t))))

(export 'matrix-init-identity)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_init_from_float ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_matrix_init_from_float" %matrix-init-from-float)
    (:pointer (:struct matrix-t))
  (m (:pointer (:struct matrix-t)))
  (value-arr (:pointer :float)))

(defun matrix-init-from-float (m &rest args)
 #+liber-documentation
 "@version{2023-12-4}
  @argument[m]{a @symbol{graphene:matrix-t} instance}
  @argument[values]{16 numbers coerced to float values}
  @return{The initialized @symbol{graphene:matrix-t} instance.}
  @short{Initializes the matrix with the given numbers.}
  @see-symbol{graphene:matrix-t}"
  (assert (<= 16 (length args)))
  (cffi:with-foreign-object (values-ar :float 16)
    (iter (for i from 0 below 16)
          (for value in args)
          (setf (cffi:mem-aref values-ar :float i)
                (coerce value 'single-float)))
    (%matrix-init-from-float m values-ar)
    m))

(export 'matrix-init-from-float)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_init_from_vec4 ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_matrix_init_from_vec4" matrix-init-from-vec4)
    (:pointer (:struct matrix-t))
 #+liber-documentation
 "@version{#2023-12-4}
  @argument[m]{a @symbol{graphene:matrix-t} instance}
  @argument[v0]{a @symbol{graphene:vec4-t} instance with a row vector}
  @argument[v1]{a @symbol{graphene:vec4-t} instance with a row vector}
  @argument[v2]{a @symbol{graphene:vec4-t} instance with a row vector}
  @argument[v3]{a @symbol{graphene:vec4-t} instance with a row vector}
  @return{The initialized @symbol{graphene:matrix-t} instance.}
  @short{Initializes the matrix with the given four row vectors.}
  @see-symbol{graphene:matrix-t}
  @see-symbol{graphene:vec4-t}"
  (m (:pointer (:struct matrix-t)))
  (v0 (:pointer (:struct vec4-t)))
  (v1 (:pointer (:struct vec4-t)))
  (v2 (:pointer (:struct vec4-t)))
  (v3 (:pointer (:struct vec4-t))))

(export 'matrix-init-from-vec4)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_init_from_matrix ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_matrix_init_from_matrix" matrix-init-from-matrix)
    (:pointer (:struct matrix-t))
 #+liber-documentation
 "@version{#2023-12-4}
  @argument[m]{a @symbol{graphene:matrix-t} instance}
  @argument[src]{a @symbol{graphene:matrix.t} instance}
  @return{The initialized @symbol{graphene:matrix-t} instance.}
  @short{Initializes the matrix using the values of the given matrix.}
  @see-symbol{graphene:matrix-t}"
  (m (:pointer (:struct matrix-t)))
  (src (:pointer (:struct matrix-t))))

(export 'matrix-init-from-matrix)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_init_from_2d ()
;;; ----------------------------------------------------------------------------

(defun matrix-init-from-2d (m xx yx xy yy x0 y0)
 #+liber-documentation
 "@version{#2023-12-4}
  @argument[m]{a @symbol{graphene:matrix-t} instance}
  @argument[xx]{a number coerced to a double float with the xx member}
  @argument[yx]{a number coerced to a double float with the yx member}
  @argument[xy]{a number coerced to a double float with the xy member}
  @argument[yy]{a number coerced to a double float with the yy member}
  @argument[x0]{a number coerced to a double float with the x0 member}
  @argument[y0]{a number coerced to a double float with the y0 member}
  @return{The initialized @symbol{graphene:matrix-t} instance.}
  @begin{short}
    Initializes the matrix from the values of an affine transformation matrix.
  @end{short}
  The arguments map to the following matrix layout:
  @begin{pre}
⎛ xx  yx ⎞   ⎛  a   b  0 ⎞
⎜ xy  yy ⎟ = ⎜  c   d  0 ⎟
⎝ x0  y0 ⎠   ⎝ tx  ty  1 ⎠
  @end{pre}
  This function can be used to convert between an affine matrix type from
  other libraries and a @symbol{graphene:matrix-t} instance.
  @see-symbol{graphene:matrix-t}"
  (cffi:foreign-funcall "graphene_matrix_init_from_2d"
                        (:pointer (:struct matrix-t)) m
                        :double (coerce xx 'double-float)
                        :double (coerce yx 'double-float)
                        :double (coerce xy 'double-float)
                        :double (coerce yy 'double-float)
                        :double (coerce x0 'double-float)
                        :double (coerce y0 'double-float)
                        (:pointer (:struct matrix-t))))

(export 'matrix-init-from-2d)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_init_perspective ()
;;; ----------------------------------------------------------------------------

(defun matrix-init-perspective (matrix fovy aspect znear zfar)
 #+liber-documentation
 "@version{#2022-9-29}
  @argument[matrix]{a @symbol{graphene:matrix-t} instance}
  @argument[fovy]{a number coerced to a float with the field of view angle, in
    degrees}
  @argument[aspect]{a number coerced to a float with the aspect value}
  @argument[znear]{a number coerced to a float with near z plane}
  @argument[zfar]{a number coerced to a float with the far z plane}
  @return{The initialized @symbol{graphene:matrix-t} instance.}
  @begin{short}
    Initializes the matrix with a perspective projection.
  @end{short}
  @see-symbol{graphene:matrix-t}"
  (cffi:foreign-funcall "graphene_matrix_init_perspective"
                        (:pointer (:struct matrix-t)) matrix
                        :float (coerce fovy 'single-float)
                        :float (coerce aspect 'single-float)
                        :float (coerce znear 'single-float)
                        :float (coerce zfar 'single-float)
                        (:pointer (:struct matrix-t))))

(export 'matrix-init-perspective)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_init_ortho ()
;;; ----------------------------------------------------------------------------

(defun matrix-init-ortho (m left right top bottom znear zfar)
 #+liber-documentation
 "@version{#2022-9-29}
  @argument[matrix]{a @symbol{graphene:matrix-t} instance}
  @argument[left]{a number coerced to a float with the left edge of the
    clipping plane}
  @argument[right]{a number coerced to a float with the right edge of the
    clipping plane}
  @argument[top]{a number coerced to a float with the top edge of the
    clipping plane}
  @argument[bottom]{a number coerced to a float with the bottom edge of the
    clipping plane}
  @argument[znear]{a number coerced to a float with the distance of the near
    clipping plane}
  @argument[zfar]{a number coerced to a float with the distance of the far
    clipping plane}
  @return{The initialized @symbol{graphene:matrix-t} instance.}
  @short{Initializes the matrix with an orthographic projection.}
  @see-symbol{graphene:matrix-t}"
  (cffi:foreign-funcall "graphene_matrix_init_ortho"
                        (:pointer (:struct matrix-t)) m
                        :float (coerce left 'single-float)
                        :float (coerce right 'single-float)
                        :float (coerce top 'single-float)
                        :float (coerce bottom 'single-float)
                        :float (coerce znear 'single-float)
                        :float (coerce zfar 'single-float)
                        (:pointer (:struct matrix-t))))

(export 'matrix-init-ortho)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_init_look_at ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_matrix_init_look_at" matrix-init-look-at)
    (:pointer (:struct matrix-t))
 #+liber-documentation
 "@version{#2022-9-29}
  @argument[matrix]{a @symbol{graphene:matrix-t} instance}
  @argument[eye]{a @symbol{graphene:vec3-t} instance with vector describing the
    position to look from}
  @argument[center]{a @symbol{graphene:vec3-t} instance with vector describing
    the position at}
  @argument[up]{a @symbol{graphene:vec3-t} instance with vector describing the
    world's upward direction, usually, this is the y axis vector returned from
    the @fun{graphene:vec3-y-axis} function}
  @return{The initialized @symbol{graphene:matrix-t} instance.}
  @begin{short}
    Initializes matrix so that it positions the \"camera\" at the given eye
    coordinates towards an object at the center coordinates.
  @end{short}
  The top of the camera is aligned to the direction of the up vector.

  Before the transform, the camera is assumed to be placed at the origin,
  looking towards the negative Z axis, with the top side of the camera facing
  in the direction of the Y axis and the right side in the direction of the X
  axis.

  In theory, one could use m to transform a model of such a camera into
  world-space. However, it is more common to use the inverse of @arg{matrix} to
  transform another object from world coordinates to the view coordinates of
  the camera. Typically you would then apply the camera projection transform to
  get from view to screen coordinates.
  @see-symbol{graphene:matrix-t}
  @see-symbol{graphene:vec3-t}"
  (matrix (:pointer (:struct matrix-t)))
  (eye (:pointer (:struct vec3-t)))
  (center (:pointer (:struct vec3-t)))
  (up (:pointer (:struct vec3-t))))

(export 'matrix-init-look-at)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_init_frustum ()
;;; ----------------------------------------------------------------------------

(defun matrix-init-frustum (m left right bottom top znear zfar)
 #+liber-documentation
 "@version{#2022-9-29}
  @argument[matrix]{a @symbol{graphene:matrix-t} instance}
  @argument[left]{a number coerced to a float with the left edge of the
    clipping plane}
  @argument[right]{a number coerced to a float with the right edge of the
    clipping plane}
  @argument[top]{a number coerced to a float with the top edge of the
    clipping plane}
  @argument[bottom]{a number coerced to a float with the bottom edge of the
    clipping plane}
  @argument[znear]{a number coerced to a float with the distance of the near
    clipping plane}
  @argument[zfar]{a number coerced to a float with the distance of the far
    clipping plane}
  @return{The initialized @symbol{graphene:matrix-t} instance.}
  @short{Initializes the matrix compatible with a frustum.}
  @see-symbol{graphene:matrix-t}
  @see-symbol{graphene:frustum-t}"
  (cffi:foreign-funcall "graphene_matrix_init_frustum"
                        (:pointer (:struct matrix-t)) m
                        :float (coerce left 'single-float)
                        :float (coerce right 'single-float)
                        :float (coerce bottom 'single-float)
                        :float (coerce top 'single-float)
                        :float (coerce znear 'single-float)
                        :float (coerce zfar 'single-float)
                        (:pointer (:struct matrix-t))))

(export 'matrix-init-frustum)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_init_scale ()
;;; ----------------------------------------------------------------------------

(defun matrix-init-scale (m x y z)
 #+liber-documentation
 "@version{#2022-9-29}
  @argument[matrix]{a @symbol{graphene:matrix-t} instance}
  @argument[x]{a number coerced to a float with the scale factor on the x axis}
  @argument[y]{a number coerced to a float with the scale factor on the y axis}
  @argument[z]{a number coerced to a float with the scale factor on the z axis}
  @return{The initialized @symbol{graphene:matrix-t} instance.}
  @short{Initializes the matrix with the given scaling factors.}
  @see-symbol{graphene:matrix-t}"
  (cffi:foreign-funcall "graphene_matrix_init_scale"
                        (:pointer (:struct matrix-t)) m
                        :float (coerce x 'single-float)
                        :float (coerce y 'single-float)
                        :float (coerce z 'single-float)
                        (:pointer (:struct matrix-t))))

(export 'matrix-init-scale)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_init_translate ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_matrix_init_translate" matrix-init-translate)
    (:pointer (:struct matrix-t))
 #+liber-documentation
 "@version{#2022-9-29}
  @argument[matrix]{a @symbol{graphene:matrix-t} instance}
  @argument[point]{a @symbol{graphene:point3d-t} instance with the translation
    coordinates}
  @return{The initialized @symbol{graphene:matrix-t} instance.}
  @short{Initializes the matrix with a translation to given coordinates.}
  @see-symbol{graphene:matrix-t}"
  (m (:pointer (:struct matrix-t)))
  (p (:pointer (:struct point3d-t))))

(export 'matrix-init-translate)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_init_rotate ()
;;; ----------------------------------------------------------------------------

(defun matrix-init-rotate (matrix angle axis)
 #+liber-documentation
 "@version{#2022-9-29}
  @argument[matrix]{a @symbol{graphene:matrix-t} instance}
  @argument[angle]{a number coerced to a float with the rotation angle,
    in degrees}
  @argument[axis]{a @symbol{graphene:vec3-t} instance with the axis vector}
  @return{The initialized @symbol{graphene:matrix-t} instance.}
  @begin{short}
    Initializes the matrix to represent a rotation of angle degrees on the axis
    represented by the axis vector.
  @end{short}
  @see-symbol{graphene:matrix-t}"
  (cffi:foreign-funcall "graphene_matrix_init_rotate"
                        (:pointer (:struct matrix-t)) matrix
                        :float (coerce angle 'single-float)
                        (:pointer (:struct vec3-t)) axis
                        (:pointer (:struct matrix-t))))

(export 'matrix-init-rotate)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_init_skew ()
;;; ----------------------------------------------------------------------------

(defun matrix-init-skew (m xskew yskew)
 #+liber-documentation
 "@version{#2022-9-29}
  @argument[matrix]{a @symbol{graphene:matrix-t} instance}
  @argument[xskew]{a number coerced to a float with the skew factor, in
    radians, on the x axis}
  @argument[yskew]{a number coerced to a float with the skew factor, in
    radians, on the y axis}
  @return{The initialized @symbol{graphene:matrix-t} instance.}
  @begin{short}
    Initializes the matrix with a skew transformation with the given factors.
  @end{short}
  @see-symbol{graphene:matrix-t}"
  (cffi:foreign-funcall "graphene_matrix_init_skew"
                        (:pointer (:struct matrix-t)) m
                        :float (coerce xskew 'single-float)
                        :float (coerce yskew 'single-float)
                        (:pointer (:struct matrix-t))))

(export 'matrix-init-skew)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_is_identity ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_matrix_is_identity" matrix-is-identity) :bool
 #+liber-documentation
 "@version{#2023-10-31}
  @argument[matrix]{a @symbol{graphene:matrix-t} instance}
  @return{@em{True} if the matrix is the identity matrix.}
  @short{Checks whether the given matrix is the identity matrix.}
  @see-symbol{graphene:matrix-t}"
  (matrix (:pointer (:struct matrix-t))))

(export 'matrix-is-identity)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_is_2d ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_matrix_is_2d" matrix-is-2d) :bool
 #+liber-documentation
 "@version{#2023-10-31}
  @argument[matrix]{a @symbol{graphene:matrix-t} instance}
  @return{@em{True} if the matrix is compatible with an affine transformation
    matrix.}
  @begin{short}
    Checks whether the given matrix is compatible with an a 2D affine
    transformation matrix.
  @end{short}
  @see-symbol{graphene:matrix-t}"
  (matrix (:pointer (:struct matrix-t))))

(export 'matrix-is-2d)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_is_backface_visible ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_matrix_is_backface_visible" matrix-is-backface-visible)
    :bool
 #+liber-documentation
 "@version{#2023-10-31}
  @argument[matrix]{a @symbol{graphene:matrix-t} instance}
  @return{@em{True} if the back face of the matrix is visible.}
  @short{Checks whether the matrix has a visible back face.}
  @see-symbol{graphene:matrix-t}"
  (matrix (:pointer (:struct matrix-t))))

(export 'matrix-is-backface-visible)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_is_singular ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_matrix_is_singular" matrix-is-singular) :bool
 #+liber-documentation
 "@version{#2023-10-31}
  @argument[matrix]{a @symbol{graphene:matrix-t} instance}
  @return{@em{True} if the matrix is singular.}
  @short{Checks whether the matrix is singular.}
  @see-symbol{graphene:matrix-t}"
  (matrix (:pointer (:struct matrix-t))))

(export 'matrix-is-singular)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_to_float ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_matrix_to_float" %matrix-to-float) :void
  (matrix (:pointer (:struct matrix-t)))
  (values-ar (:pointer :float)))

(defun matrix-to-float (matrix)
 #+liber-documentation
 "@version{2023-12-3}
  @argument[matrix]{a @symbol{graphene:matrix-t} instance}
  @return{The list with the floating point values.}
  @begin{short}
    Converts the matrix to a list of floating point values.
  @end{short}
  @see-symbol{graphene:matrix-t}"
  (cffi:with-foreign-object (values-ar :float 16)
    (%matrix-to-float matrix values-ar)
    (iter (for i from 0 below 16)
          (collect (cffi:mem-aref values-ar :float i)))))

(export 'matrix-to-float)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_to_2d ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_matrix_to_2d" %matrix-to-2d) :bool
  (m (:pointer (:struct matrix-t)))
  (xx (:pointer :double))
  (yx (:pointer :double))
  (xy (:pointer :double))
  (yy (:pointer :double))
  (x0 (:pointer :double))
  (y0 (:pointer :double)))

(defun matrix-to-2d (m)
 #+liber-documentation
 "@version{#2024-1-20}
  @syntax{(matrix-to-2d matrix) => (list xx yx xy yy x0 y0)}
  @argument[matrix]{a @symbol{graphene:matrix-t} instance}
  @argument[xx]{a float with the xx member}
  @argument[yx]{a float with the yx member}
  @argument[xy]{a float with the xy member}
  @argument[yy]{a float with the yy member}
  @argument[x0]{a float with the x0 member}
  @argument[y0]{a float with the y0 member}
  @return{The list with the floating point values, or @code{nil} if the
    matrix is not compatible with an affine transformation matrix.}
  @begin{short}
    Converts the matrix to an affine transformation matrix, if the given matrix
    is compatible.
  @end{short}
  The returned values have the following layout:
  @begin{pre}
⎛ xx  yx ⎞   ⎛  a   b  0 ⎞
⎜ xy  yy ⎟ = ⎜  c   d  0 ⎟
⎝ x0  y0 ⎠   ⎝ tx  ty  1 ⎠
  @end{pre}
  This function can be used to convert between a @symbol{graphene:matrix-t}
  instance and an affine matrix type from other libraries.
  @see-symbol{graphene:matrix-t}"
  (cffi:with-foreign-objects ((xx :double)
                              (yx :double)
                              (xy :double)
                              (yy :double)
                              (x0 :double)
                              (y0 :double))
    (when (%matrix-to-2d m xx yx xy yy x0 y0)
      (values (cffi:mem-ref xx :double)
              (cffi:mem-ref yx :double)
              (cffi:mem-ref xy :double)
              (cffi:mem-ref yy :double)
              (cffi:mem-ref x0 :double)
              (cffi:mem-ref y0 :double)))))

(export 'matrix-to-2d)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_get_row ()
;;; ----------------------------------------------------------------------------

(defun matrix-row (matrix index result)
 #+liber-documentation
 "@version{#2022-9-30}
  @argument[matrix]{a @symbol{graphene:matrix-t} instance}
  @argument[index]{an unsigned integer with the index of the row vector,
    between 0 to 3}
  @argument[result]{a @symbol{graphene:vec4-t} instance}
  @return{The @symbol{graphene:vec4-t} instance  with the row vector.}
  @begin{short}
    Retrieves the given row vector at @arg{index} inside the matrix.
  @end{short}
  @see-symbol{graphene:matrix-t}"
  (cffi:foreign-funcall "graphene_matrix_get_row"
                        (:pointer (:struct matrix-t)) matrix
                        :int index
                        (:pointer (:struct vec4-t)) result
                        :void)
  result)

(export 'matrix-row)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_get_value ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_matrix_get_value" matrix-value) :float
 #+liber-documentation
 "@version{#2022-9-30}
  @argument[matrix]{a @symbol{graphene:matrix-t} instance}
  @argument[row]{an unsigned integer with the row index}
  @argument[col]{an unsigned integer with the column index}
  @return{The float with the value at the given indices.}
  @begin{short}
    Retrieves the given row vector at @arg{index} inside the matrix.
  @end{short}
  @see-symbol{graphene:matrix-t}"
  (matrix (:pointer (:struct matrix-t)))
  (row :int)
  (col :int))

(export 'matrix-value)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_multiply ()
;;; ----------------------------------------------------------------------------

(defun matrix-multiply (a b result)
 #+liber-documentation
 "@version{#2022-9-30}
  @argument[a]{a @symbol{graphene:matrix-t} instance}
  @argument[b]{a @symbol{graphene:matrix-t} instance}
  @argument[result]{a @symbol{graphene:matrix-t} instance}
  @return{The @symbol{graphene:matrix-t} instance with the matrix result.}
  @begin{short}
    Multiplies two matrices.
  @end{short}
  Matrix multiplication is not commutative in general. The order of the factors
  matters. The product of this multiplication is (a × b ).
  @see-symbol{graphene:matrix-t}"
  (cffi:foreign-funcall "graphene_matrix_multiply"
                        (:pointer (:struct matrix-t)) a
                        (:pointer (:struct matrix-t)) b
                        (:pointer (:struct matrix-t)) result
                        :void)
  result)

(export 'matrix-multiply)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_determinant ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_matrix_determinant" matrix-determinant) :float
 #+liber-documentation
 "@version{#2022-9-30}
  @argument[matrix]{a @symbol{graphene:matrix-t} instance}
  @argument[result]{a float with the value of the determinat}
  @return{The @symbol{graphene:matrix-t} instance with the matrix result.}
  @begin{short}
    Computes the determinant of the given matrix.
  @end{short}
  Matrix multiplication is not commutative in general. The order of the factors
  matters. The product of this multiplication is (a × b ).
  @see-symbol{graphene:matrix-t}"
  (matrix (:pointer (:struct matrix-t))))

(export 'matrix-determinant)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_transform_vec4 ()
;;; ----------------------------------------------------------------------------

(defun matrix-transform-vec4 (matrix vector result)
 #+liber-documentation
 "@version{#2022-9-30}
  @argument[matrix]{a @symbol{graphene:matrix-t} instance}
  @argument[vector]{a @symbol{graphene:vec4-t} instance}
  @argument[result]{a @symbol{graphene:vec4-t} instance}
  @return{The @symbol{graphene:vec4-t} instance with the result.}
  @short{Transforms the given vector using the given matrix.}
  @see-symbol{graphene:matrix-t}
  @see-symbol{graphene:vec4-t}"
  (cffi:foreign-funcall "graphene_matrix_transform_vec4"
                        (:pointer (:struct matrix-t)) matrix
                        (:pointer (:struct vec4-t)) vector
                        (:pointer (:struct vec4-t)) result
                        :void)
  result)

(export 'matrix-transform-vec4)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_transform_vec3 ()
;;; ----------------------------------------------------------------------------

(defun matrix-transform-vec3 (matrix vector result)
 #+liber-documentation
 "@version{#2022-9-30}
  @argument[matrix]{a @symbol{graphene:matrix-t} instance}
  @argument[vector]{a @symbol{graphene:vec3-t} instance}
  @argument[result]{a @symbol{graphene:vec3-t} instance}
  @return{The @symbol{graphene:vec3-t} instance with the result.}
  @begin{short}
    Transforms the given vector using the given matrix.
  @end{short}
  This function will multiply the x, y, and z row vectors of the matrix
  @arg{matrix} with the corresponding components of the vector @arg{vector}.
  The w row vector will be ignored.
  @see-symbol{graphene:matrix-t}
  @see-symbol{graphene:vec3-t}"
  (cffi:foreign-funcall "graphene_matrix_transform_vec3"
                        (:pointer (:struct matrix-t)) matrix
                        (:pointer (:struct vec3-t)) vector
                        (:pointer (:struct vec3-t)) result
                        :void)
  result)

(export 'matrix-transform-vec3)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_transform_point ()
;;; ----------------------------------------------------------------------------

(defun matrix-transform-point (matrix point result)
 #+liber-documentation
 "@version{#2022-9-30}
  @argument[matrix]{a @symbol{graphene:matrix-t} instance}
  @argument[point]{a @symbol{graphene:point-t} instance}
  @argument[result]{a @symbol{graphene:point-t} instance}
  @return{The @symbol{graphene:point-t} instance with the result.}
  @begin{short}
    Transforms the given point using the given matrix.
  @end{short}
  Unlike the @fun{graphene:matrix-transform-vec3} function, this function will
  take into account the fourth row vector of the matrix when computing the dot
  product of each row vector of the matrix.
  @see-symbol{graphene:matrix-t}
  @see-symbol{graphene:point-t}
  @see-function{graphene:matrix-transform-vec3}"
  (cffi:foreign-funcall "graphene_matrix_transform_point"
                        (:pointer (:struct matrix-t)) matrix
                        (:pointer (:struct point-t)) point
                        (:pointer (:struct point-t)) result
                        :void)
  result)

(export 'matrix-transform-point)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_transform_point3d ()
;;; ----------------------------------------------------------------------------

(defun matrix-transform-point3d (matrix point result)
 #+liber-documentation
 "@version{#2022-9-30}
  @argument[matrix]{a @symbol{graphene:matrix-t} instance}
  @argument[point]{a @symbol{graphene:point3d-t} instance}
  @argument[result]{a @symbol{graphene:point3d-t} instance}
  @return{The @symbol{graphene:point3d-t} instance with the result.}
  @begin{short}
    Transforms the given point using the given matrix.
  @end{short}
  Unlike the @fun{graphene:matrix-tansform-vec3} function, this function will
  take into account the fourth row vector of the matrix when computing the dot
  product of each row vector of the matrix.
  @see-symbol{graphene:matrix-t}
  @see-symbol{graphene:point3d-t}"
  (cffi:foreign-funcall "graphene_matrix_transform_point3d"
                        (:pointer (:struct matrix-t)) matrix
                        (:pointer (:struct point3d-t)) point
                        (:pointer (:struct point3d-t)) result
                        :void)
  result)

(export 'matrix-transform-point3d)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_transform_rect ()
;;; ----------------------------------------------------------------------------

(defun matrix-transform-rect (matrix rect result)
 #+liber-documentation
 "@version{#2022-9-30}
  @argument[matrix]{a @symbol{graphene:matrix-t} instance}
  @argument[rect]{a @symbol{graphene:rect-t} instance}
  @argument[result]{a @symbol{graphene:quad-t} instance}
  @return{The @symbol{graphene:quad-t} instance with the result.}
  @begin{short}
    Transforms each corner of the rect using the given matrix.
  @end{short}
  The result is a coplanar quadrilateral. See also the
  @fun{graphene:matrix-transform-point} function.
  @see-symbol{graphene:matrix-t}
  @see-symbol{graphene:quad-t}
  @see-function{graphene:matrix-transform-point}"
  (cffi:foreign-funcall "graphene_matrix_transform_rect"
                        (:pointer (:struct matrix-t)) matrix
                        (:pointer (:struct rect-t)) rect
                        (:pointer (:struct rect-t)) result
                        :void)
  result)

(export 'matrix-transform-rect)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_transform_bounds ()
;;; ----------------------------------------------------------------------------

(defun matrix-transform-bounds (matrix rect result)
 #+liber-documentation
 "@version{#2022-9-30}
  @argument[matrix]{a @symbol{graphene:matrix-t} instance}
  @argument[rect]{a @symbol{rect-t} instance}
  @argument[result]{a @symbol{rect-t} instance}
  @return{The @symbol{rect-t} instance with the bounds the transformed rectangle.}
  @begin{short}
    Transforms each corner of the rectangle using the given matrix.
  @end{short}
  The result is the axis aligned bounding rectangle containing the coplanar
  quadrilateral. See also the @fun{graphene:matrix-transform-point} function.
  @see-symbol{graphene:matrix-t}
  @see-symbol{rect-t}
  @see-function{matrix-transform-point}"
  (cffi:foreign-funcall "graphene_matrix_transform_bounds"
                        (:pointer (:struct matrix-t)) matrix
                        (:pointer (:struct rect-t)) rect
                        (:pointer (:struct rect-t)) result
                        :void)
  result)

(export 'matrix-transform-bounds)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_transform_box ()
;;; ----------------------------------------------------------------------------

(defun matrix-transform-box (matrix box result)
 #+liber-documentation
 "@version{#2022-9-30}
  @argument[matrix]{a @symbol{graphene:matrix-t} instance}
  @argument[box]{a @symbol{graphene:box-t} instance}
  @argument[result]{a @symbol{graphene:box-t} instance}
  @return{The @symbol{box-t} instance with the bounds the transformed box.}
  @begin{short}
    Transforms the vertices of the box using the given matrix.
  @end{short}
  The result is the axis aligned bounding box containing the transformed
  vertices.
  @see-symbol{graphene:matrix-t}
  @see-symbol{graphene:box-t}"
  (cffi:foreign-funcall "graphene_matrix_transform_box"
                        (:pointer (:struct matrix-t)) matrix
                        (:pointer (:struct rect-t)) box
                        (:pointer (:struct rect-t)) result
                        :void)
  result)

(export 'matrix-transform-box)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_transform_sphere ()
;;; ----------------------------------------------------------------------------

(defun matrix-transform-sphere (matrix sphere result)
 #+liber-documentation
 "@version{#2022-9-30}
  @argument[matrix]{a @symbol{graphene:matrix-t} instance}
  @argument[sphere]{a @symbol{graphene:sphere-t} instance}
  @argument[result]{a @symbol{graphene:sphere-t} instance}
  @return{The @symbol{graphene:sphere-t} instance with the bounds of the
    transformed sphere.}
  @begin{short}
    Transforms the sphere using the given matrix.
  @end{short}
  The result is the bounding sphere containing the transformed sphere.
  @see-symbol{graphene:matrix-t}
  @see-symbol{graphene:sphere-t}"
  (cffi:foreign-funcall "graphene_matrix_transform_sphere"
                        (:pointer (:struct matrix-t)) matrix
                        :pointer sphere  ; sphere-t not known at this point
                        :pointer result
                        :void)
  result)

(export 'matrix-transform-sphere)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_transform_ray ()
;;; ----------------------------------------------------------------------------

(defun matrix-transform-ray (matrix ray result)
 #+liber-documentation
 "@version{#2022-9-30}
  @argument[matrix]{a @symbol{graphene:matrix-t} instance}
  @argument[ray]{a @symbol{graphene:ray-t} instance}
  @argument[result]{a @symbol{graphene:ray-t} instance}
  @return{The @symbol{graphene:ray-t} instance with the transformed ray.}
  @begin{short}
    Transforms the ray using the given matrix.
  @end{short}
  @see-symbol{graphene:matrix-t}
  @see-symbol{graphene:ray-t}"
  (cffi:foreign-funcall "graphene_matrix_transform_ray"
                        (:pointer (:struct matrix-t)) matrix
                        :pointer ray     ; ray-t not known at this point
                        :pointer result
                        :void)
  result)

(export 'matrix-transform-ray)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_project_point ()
;;; ----------------------------------------------------------------------------

(defun matrix-project-point (matrix point result)
 #+liber-documentation
 "@version{#2022-9-30}
  @argument[matrix]{a @symbol{graphene:matrix-t} instance}
  @argument[point]{a @symbol{graphene:point-t} instance}
  @argument[result]{a @symbol{graphene:point-t} instance}
  @return{The @symbol{graphene:point-t} instance with the projected point.}
  @begin{short}
    Projects the point using the given matrix.
  @end{short}
  @see-symbol{graphene:matrix-t}
  @see-symbol{graphene:point-t}"
  (cffi:foreign-funcall "graphene_matrix_project_point"
                        (:pointer (:struct matrix-t)) matrix
                        (:pointer (:struct point-t)) point
                        (:pointer (:struct point-t)) result
                        :void)
  result)

(export 'matrix-project-point)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_project_rect_bounds ()
;;; ----------------------------------------------------------------------------

(defun matrix-project-rect-bounds (matrix rect result)
 #+liber-documentation
 "@version{#2022-9-30}
  @argument[matrix]{a @symbol{graphene:matrix-t} instance}
  @argument[rect]{a @symbol{graphene:rect-t} instance}
  @argument[result]{a @symbol{graphene:rect-t} instance}
  @return{The @symbol{graphene:rect-t} instance with the projected rectangle.}
  @begin{short}
    Projects the rectangle using the given matrix.
  @end{short}
  The resulting rectangle is the axis aligned bounding rectangle capable of
  fully containing the projected rectangle.
  @see-symbol{graphene:matrix-t}
  @see-symbol{graphene:rect-t}"
  (cffi:foreign-funcall "graphene_matrix_project_rect_bounds"
                        (:pointer (:struct matrix-t)) matrix
                        (:pointer (:struct rect-t)) rect
                        (:pointer (:struct rect-t)) result
                        :void)
  result)

(export 'matrix-project-rect-bounds)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_project_rect ()
;;; ----------------------------------------------------------------------------

(defun matrix-project-rect (matrix rect result)
 #+liber-documentation
 "@version{#2022-9-30}
  @argument[matrix]{a @symbol{graphene:matrix-t} instance}
  @argument[rect]{a @symbol{graphene:rect-t} instance}
  @argument[result]{a @symbol{graphene:rect-t} instance}
  @return{The @symbol{graphene:rect-t} instance with the projected rectangle.}
  @begin{short}
    Projects all corners of the rectangle using the given matrix.
  @end{short}
  See also the @fun{graphene:matrix-project-point} function.
  @see-symbol{graphene:matrix-t}
  @see-symbol{graphene:rect-t}"
  (cffi:foreign-funcall "graphene_matrix_project_rect"
                        (:pointer (:struct matrix-t)) matrix
                        (:pointer (:struct rect-t)) rect
                        :pointer result ; quad-t not known at this point
                        :void)
  result)

(export 'matrix-project-rect)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_untransform_point ()
;;; ----------------------------------------------------------------------------

(defun matrix-untransform-point (matrix point bounds result)
 #+liber-documentation
 "@version{#2022-9-30}
  @argument[matrix]{a @symbol{graphene:matrix-t} instance}
  @argument[point]{a @symbol{graphene:point-t} instance}
  @argument[bounds]{a @symbol{graphene:rect-t} instance}
  @argument[result]{a @symbol{graphene:rect-t} instance}
  @return{The @symbol{graphene:rect-t} instance with the projected rectangle, if
    sucessfully untransformed, otherwise @code{nil}.}
  @begin{short}
    Undoes the transformation of the point using the given matrix, within the
    given axis aligned rectangular bounds.
  @end{short}
  @see-symbol{graphene:matrix-t}
  @see-symbol{graphene:point-t}
  @see-symbol{graphene:rect-t}"
  (when (cffi:foreign-funcall "graphene_untransform_point"
                              (:pointer (:struct matrix-t)) matrix
                              (:pointer (:struct point-t)) point
                              (:pointer (:struct rect-t)) bounds
                              (:pointer (:struct point-t)) result
                              :bool)
    result))

(export 'matrix-untransform-point)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_untransform_bounds ()
;;; ----------------------------------------------------------------------------

(defun matrix-untransform-bounds (matrix rect bounds result)
 #+liber-documentation
 "@version{#2022-9-30}
  @argument[matrix]{a @symbol{graphene:matrix-t} instance}
  @argument[rect]{a @symbol{graphene:rect-t} instance}
  @argument[bounds]{a @symbol{graphene:rect-t} instance}
  @argument[result]{a @symbol{graphene:rect-t} instance}
  @return{The @symbol{graphene:rect-t} instance with the untransformed rectangle.}
  @begin{short}
    Undoes the transformation on the corners of the rectanble using the given
    matrix, within the given axis aligned rectangular bounds.
  @end{short}
  @see-symbol{graphene:matrix-t}
  @see-symbol{graphene:rect-t}"
  (cffi:foreign-funcall "graphene_untransform_bounds"
                        (:pointer (:struct matrix-t)) matrix
                        (:pointer (:struct rect-t)) rect
                        (:pointer (:struct rect-t)) bounds
                        (:pointer (:struct rect-t)) result
                        :void)
  result)

(export 'matrix-untransform-bounds)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_unproject_point3d ()
;;; ----------------------------------------------------------------------------

(defun matrix-unproject-point3d (projection modelview point result)
 #+liber-documentation
 "@version{#2022-9-30}
  @argument[projection]{a @symbol{graphene:matrix-t} instance}
  @argument[modelview]{a @symbol{graphene:matrix-t} instance}
  @argument[point]{a @symbol{graphene:point3d-t} instance}
  @argument[result]{a @symbol{graphene:point3d-t} instance}
  @return{The @symbol{graphene:point3d-t} instance with the unprojected point.}
  @begin{short}
    Unprojects the given point using the projection matrix and a modelview
    matrix.
  @end{short}
  @see-symbol{graphene:matrix-t}
  @see-symbol{graphene:point3d-t}"
  (cffi:foreign-funcall "graphene_matrix_unproject_point3d"
                        (:pointer (:struct matrix-t)) projection
                        (:pointer (:struct matrix-t)) modelview
                        (:pointer (:struct point3d-t)) point
                        (:pointer (:struct point3d-t)) result
                        :void)
  result)

(export 'matrix-unproject-point3d)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_translate ()
;;; ----------------------------------------------------------------------------

(defun matrix-translate (matrix position)
 #+liber-documentation
 "@version{#2022-9-30}
  @argument[matrix]{a @symbol{graphene:matrix-t} instance}
  @argument[position]{a @symbol{graphene:point3d-t} instance}
  @return{The @symbol{graphene:matrix} instance with the result.}
  @begin{short}
    Adds a translation transformation to the matrix using the coordinates of
    the given point.
  @end{short}
  This is the equivalent of calling the @fun{graphene:matrix-init-translate}
  function and then multiplying the matrix with the translation matrix.
  @see-symbol{graphene:matrix-t}
  @see-symbol{graphene:point3d-t}"
  (cffi:foreign-funcall "graphene_matrix_translate"
                        (:pointer (:struct matrix-t)) matrix
                        (:pointer (:struct point3d-t)) position
                        :void)
  matrix)

(export 'matrix-translate)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_rotate ()
;;; ----------------------------------------------------------------------------

(defun matrix-rotate (matrix angle axis)
 #+liber-documentation
 "@version{#2022-9-30}
  @argument[matrix]{a @symbol{graphene:matrix-t} instance}
  @argument[angle]{a float with the rotation angle, in degrees}
  @argument[axis]{a @symbol{graphene:vec3-t} instance with rotation axis}
  @return{The @symbol{graphene:matrix-t} instance with the result.}
  @begin{short}
    Adds a rotation transformation to the matrix, using the given angle and
    axis vector.
  @end{short}
  This is the equivalent of calling the @fun{graphene:matrix-init-rotate}
  function and then multiplying the matrix with the rotation matrix.
  @see-symbol{gaphene:matrix-t}
  @see-symbol{graphene:vec3-t}"
  (cffi:foreign-funcall "graphene_matrix_rotate"
                        (:pointer (:struct matrix-t)) matrix
                        :float (coerce angle 'single-float)
                        (:pointer (:struct vec3-t)) axis
                        :void)
  matrix)

(export 'matrix-rotate)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_rotate_x ()
;;; ----------------------------------------------------------------------------

(defun matrix-rotate-x (matrix angle)
 #+liber-documentation
 "@version{#2022-9-30}
  @argument[matrix]{a @symbol{graphene:matrix-t} instance}
  @argument[angle]{a float with the rotation angle, in degrees}
  @return{The @symbol{graphene:matrix} instance with the result.}
  @begin{short}
    Adds a rotation transformation around the x axis to the matrix, using the
    given angle.
  @end{short}
  See also the @fun{graphene:matrix-rotate} function.
  @see-symbol{graphene:matrix-t}"
  (cffi:foreign-funcall "graphene_matrix_rotate_x"
                        (:pointer (:struct matrix-t)) matrix
                        :float (coerce angle 'single-float)
                        :void)
  matrix)

(export 'matrix-rotate-x)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_rotate_y ()
;;; ----------------------------------------------------------------------------

(defun matrix-rotate-y (matrix angle)
 #+liber-documentation
 "@version{#2022-9-30}
  @argument[matrix]{a @symbol{graphene:matrix-t} instance}
  @argument[angle]{a float with the rotation angle, in degrees}
  @return{The @symbol{graphene:matrix-t} instance with the result.}
  @begin{short}
    Adds a rotation transformation around the y axis to the matrix, using the
    given angle.
  @end{short}
  See also the @fun{graphene:matrix-rotate} function.
  @see-symbol{graphene:matrix-t}"
  (cffi:foreign-funcall "graphene_matrix_rotate_y"
                        (:pointer (:struct matrix-t)) matrix
                        :float (coerce angle 'single-float)
                        :void)
  matrix)

(export 'matrix-rotate-y)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_rotate_z ()
;;; ----------------------------------------------------------------------------

(defun matrix-rotate-z (matrix angle)
 #+liber-documentation
 "@version{#2022-9-30}
  @argument[matrix]{a @symbol{graphene:matrix-t} instance}
  @argument[angle]{a float with the rotation angle, in degrees}
  @return{The @symbol{graphene:matrix-t} instance with the result.}
  @begin{short}
    Adds a rotation transformation around the z axis to the matrix, using the
    given angle.
  @end{short}
  See also the @fun{graphene:matrix-rotate} function.
  @see-symbol{graphene:matrix-t}"
  (cffi:foreign-funcall "graphene_matrix_rotate_z"
                        (:pointer (:struct matrix-t)) matrix
                        :float (coerce angle 'single-float)
                        :void)
  matrix)

(export 'matrix-rotate-z)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_rotate_quaternion ()
;;; ----------------------------------------------------------------------------

(defun matrix-rotate-quaternion (matrix quaternion)
 #+liber-documentation
 "@version{#2022-9-30}
  @argument[matrix]{a @symbol{graphene:matrix-t} instance}
  @argument[quaternion]{a @symbol{graphene:quaternion-t} instance}
  @begin{short}
    Adds a rotation transformation to the matrix, using the given quaternion.
  @end{short}
  This is the equivalent of calling the @fun{graphene:quaternion-to-matrix}
  function and then multiplying the matrix with the rotation matrix.
  @see-symbol{graphene:matrix-t}
  @see-symbol{graphene:quaternion-t}
  @see-function{graphene:quaternion-to-matrix}"
  (cffi:foreign-funcall "graphene_matrix_rotate_quaternion"
                        (:pointer (:struct matrix-t)) matrix
                        ;; quaternion-t not known at this point
                        :pointer quaternion
                        :void)
  matrix)

(export 'matrix-rotate-quaternion)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_rotate_euler ()
;;; ----------------------------------------------------------------------------

(defun matrix-rotate-euler (matrix euler)
 #+liber-documentation
 "@version{#2022-9-30}
  @argument[matrix]{a @symbol{graphene:matrix-t} instance}
  @argument[euler]{a @symbol{graphene:euler-t} instance}
  @return{The @symbol{graphene:matrix-t} instance with the result.}
  @begin{short}
    Adds a rotation transformation to the matrix, using the given euler
    angles.
  @end{short}
  @see-symbol{graphene:matrix-t}
  @see-symbol{graphene:euler-t}"
  (cffi:foreign-funcall "graphene_matrix_rotate_euler"
                        (:pointer (:struct matrix-t)) matrix
                        :pointer euler
                        :void)
  matrix)

(export 'matrix-rotate-euler)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_scale ()
;;; ----------------------------------------------------------------------------

(defun matrix-scale (matrix xfactor yfactor zfactor)
 #+liber-documentation
 "@version{#2022-9-30}
  @argument[matrix]{a @symbol{graphene:matrix-t} instance}
  @argument[xfactor]{a float with the scaling factor on the x axis}
  @argument[yfactor]{a float with the scaling factor on the y axis}
  @argument[zfactor]{a float with the scaling factor on the z axis}
  @return{The @symbol{graphene:matrix-t} instance with the result.}
  @begin{short}
    Adds a scaling transformation to the matrix, using the three given factors.
  @end{short}
  This is the equivalent of calling the @fun{graphene:matrix-init-scale}
  function and then multiplying the matrix with the scale matrix.
  @see-symbol{graphene:matrix-t}"
  (cffi:foreign-funcall "graphene_matrix_scale"
                        (:pointer (:struct matrix-t)) matrix
                        :float (coerce xfactor 'single-float)
                        :float (coerce yfactor 'single-float)
                        :float (coerce zfactor 'single-float)
                        :void)
  matrix)

(export 'matrix-scale)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_skew_xy ()
;;; ----------------------------------------------------------------------------

(defun matrix-skew-xy (matrix factor)
 #+liber-documentation
 "@version{#2022-9-30}
  @argument[matrix]{a @symbol{graphene:matrix-t} instance}
  @argument[factor]{a float with the skew factor}
  @return{The @symbol{graphene:matrix-t} instance with the result.}
  @short{Adds a skew of factor on the x and y axis to the given matrix.}
  @see-symbol{graphene:matrix-t}"
  (cffi:foreign-funcall "graphene_matrix_skew_xy"
                        (:pointer (:struct matrix-t)) matrix
                        :float (coerce factor 'single-float)
                        :void)
  matrix)

(export 'matrix-skew-xy)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_skew_xz ()
;;; ----------------------------------------------------------------------------

(defun matrix-skew-xz (matrix factor)
 #+liber-documentation
 "@version{#2022-9-30}
  @argument[matrix]{a @symbol{graphene:matrix-t} instance}
  @argument[factor]{a float with the skew factor}
  @return{The @symbol{graphene:matrix-t} instance with the result.}
  @begin{short}
    Adds a skew of factor on the x and z axis to the given matrix.
  @end{short}
  @see-symbol{graphene:matrix-t}"
  (cffi:foreign-funcall "graphene_matrix_skew_xz"
                        (:pointer (:struct matrix-t)) matrix
                        :float (coerce factor 'single-float)
                        :void)
  matrix)

(export 'matrix-skew-xz)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_skew_yz ()
;;; ----------------------------------------------------------------------------

(defun matrix-skew-yz (matrix factor)
 #+liber-documentation
 "@version{#2022-9-30}
  @argument[matrix]{a @symbol{graphene:matrix-t} instance}
  @argument[factor]{a float with the skew factor}
  @return{The @symbol{graphene:matrix-t} instance with the result.}
  @begin{short}
    Adds a skew of factor on the y and z axis to the given matrix.
  @end{short}
  @see-symbol{graphene:matrix-t}"
  (cffi:foreign-funcall "graphene_matrix_skew_yz"
                        (:pointer (:struct matrix-t)) matrix
                        :float (coerce factor 'single-float)
                        :void)
  matrix)

(export 'matrix-skew-yz)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_transpose ()
;;; ----------------------------------------------------------------------------

(defun matrix-transpose (matrix result)
 #+liber-documentation
 "@version{#2022-9-30}
  @argument[matrix]{a @symbol{graphene:matrix-t} instance}
  @return{The @symbol{graphene:matrix-t} instance with the transposed matrix.}
  @begin{short}
    Transposes the given matrix.
  @end{short}
  @see-symbol{graphene:matrix-t}"
  (cffi:foreign-funcall "graphene_matrix_transpose"
                        (:pointer (:struct matrix-t)) matrix
                        (:pointer (:struct matrix-t)) result
                        :void)
  matrix)

(export 'matrix-transpose)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_inverse ()
;;; ----------------------------------------------------------------------------

(defun matrix-inverse (matrix result)
 #+liber-documentation
 "@version{#2022-9-30}
  @argument[matrix]{a @symbol{graphene:matrix-t} instance}
  @return{The @symbol{graphene:matrix-t} instance with the inversed matrix,
    @code{nil} if the matrix is not invertible.}
  @short{Inverts the given matrix.}
  @see-symbol{graphene:matrix-t}"
  (when (cffi:foreign-funcall "graphene_matrix_inverse"
                              (:pointer (:struct matrix-t)) matrix
                              (:pointer (:struct matrix-t)) result
                              :bool)
    matrix))

(export 'matrix-inverse)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_perspective ()
;;; ----------------------------------------------------------------------------

(defun matrix-perspective (matrix depth result)
 #+liber-documentation
 "@version{#2022-9-30}
  @argument[matrix]{a @symbol{graphene:matrix-t} instance}
  @argument[depth]{a float with the depth of the perspective}
  @return{The @symbol{graphene:matrix-t} instance with the perpective matrix.}
  @short{Applies a perspective of depth to the matrix.}
  @see-symbol{graphene:matrix-t}"
  (cffi:foreign-funcall "graphene_matrix_perspective"
                        (:pointer (:struct matrix-t)) matrix
                        :float (coerce depth 'single-float)
                        (:pointer (:struct matrix-t)) result
                        :void)
  matrix)

(export 'matrix-perspective)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_normalize ()
;;; ----------------------------------------------------------------------------

(defun matrix-normalize (matrix result)
 #+liber-documentation
 "@version{#2022-9-30}
  @argument[matrix]{a @symbol{graphene:matrix-t} instance}
  @return{The @symbol{graphene:matrix-t} instance with the normalized matrix.}
  @short{Normalizes the given matrix.}
  @see-symbol{graphene:matrix-t}"
  (cffi:foreign-funcall "graphene_matrix_normalize"
                        (:pointer (:struct matrix-t)) matrix
                        (:pointer (:struct matrix-t)) result
                        :void)
  matrix)

(export 'matrix-normalize)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_get_x_translation ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_matrix_get_x_translation" matrix-x-translation) :float
 #+liber-documentation
 "@version{#2022-9-30}
  @argument[matrix]{a @symbol{graphene:matrix-t} instance}
  @return{The float with the translation component.}
  @begin{short}
    Retrieves the translation component on the x axis from the given matrix.
  @end{short}
  @see-symbol{graphene:matrix-t}"
  (matrix (:pointer (:struct matrix-t))))

(export 'matrix-x-translation)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_get_y_translation ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_matrix_get_y_translation" matrix-y-translation) :float
 #+liber-documentation
 "@version{#2022-9-30}
  @argument[matrix]{a @symbol{graphene:matrix-t} instance}
  @return{The float with the translation component.}
  @begin{short}
    Retrieves the translation component on the y axis from the given matrix.
  @end{short}
  @see-symbol{graphene:matrix-t}"
  (matrix (:pointer (:struct matrix-t))))

(export 'matrix-y-translation)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_get_z_translation ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_matrix_get_z_translation" matrix-z-translation) :float
 #+liber-documentation
 "@version{#2022-9-30}
  @argument[matrix]{a @symbol{graphene:matrix-t} instance}
  @return{The float with the translation component.}
  @begin{short}
    Retrieves the translation component on the z axis from the given matrix.
  @end{short}
  @see-symbol{graphene:matrix-t}"
  (matrix (:pointer (:struct matrix-t))))

(export 'matrix-z-translation)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_get_x_scale ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_matrix_get_x_scale" matrix-x-scale) :float
 #+liber-documentation
 "@version{#2022-9-30}
  @argument[matrix]{a @symbol{graphene:matrix-t} instance}
  @return{The float with the value of the scaling factor.}
  @begin{short}
    Retrieves the scaling factor on the x axis in the given matrix.
  @end{short}
  @see-symbol{graphene:matrix-t}"
  (matrix (:pointer (:struct matrix-t))))

(export 'matrix-x-scale)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_get_y_scale ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_matrix_get_y_scale" matrix-y-scale) :float
 #+liber-documentation
 "@version{#2022-9-30}
  @argument[matrix]{a @symbol{graphene:matrix-t} instance}
  @return{The float with the value of the scaling factor.}
  @begin{short}
    Retrieves the scaling factor on the y axis in the given matrix.
  @end{short}
  @see-symbol{graphene:matrix-t}"
  (matrix (:pointer (:struct matrix-t))))

(export 'matrix-y-scale)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_get_z_scale ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_matrix_get_z_scale" matrix-z-scale) :float
 #+liber-documentation
 "@version{#2022-9-30}
  @argument[matrix]{a @symbol{graphene:matrix-t} instance}
  @return{The float with the value of the scaling factor.}
  @short{Retrieves the scaling factor on the z axis in the given matrix.}
  @see-symbol{graphene:matrix-t}"
  (matrix (:pointer (:struct matrix-t))))

(export 'matrix-z-scale)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_decompose ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_matrix_decompose" matrix-decompose) :bool
 #+liber-documentation
 "@version{#2022-9-30}
  @argument[matrix]{a @symbol{graphene:matrix-t} instance}
  @argument[translate]{a @symbol{graphene:vec3-t} instance}
  @argument[scale]{a @symbol{graphene:vec3-t} instance}
  @argument[rotate]{a @symbol{graphene:quaternion-t} instance}
  @argument[shear]{a @symbol{graphene:vec3-t} instance}
  @argument[perspective]{a @symbol{graphene:vec4-t} instance}
  @return{@em{True} if the matrix could be composed.}
  @begin{short}
    Decomposes a transformation matrix into its component transformations.
  @end{short}

  The algorithm for decomposing a matrix is taken from the CSS3 Transforms
  specification. Specifically, the decomposition code is based on the equivalent
  code published in \"Graphics Gems II\", edited by Jim Arvo, and available
  online.
  @see-symbol{graphene:matrix-t}
  @see-symbol{graphene:vec3-t}
  @see-symbol{graphene:vec4-t}
  @see-symbol{graphene:quaternion-t}"
  (matrix (:pointer (:struct matrix-t)))
  (translate (:pointer (:struct vec3-t)))
  (scale (:pointer (:struct vec3-t)))
  (rotate :pointer) ; quaternion-t now known at this point
  (shear (:pointer (:struct vec3-t)))
  (perspective (:pointer (:struct vec4-t))))

(export 'matrix-decompose)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_interpolate ()
;;; ----------------------------------------------------------------------------

(defun matrix-interpolate (a b factor result)
 #+liber-documentation
 "@version{#2022-9-30}
  @argument[a]{a @symbol{graphene:matrix-t} instance}
  @argument[b]{a @symbol{graphene:vec3-t} instance}
  @argument[factor]{a double float with the linear interpolation factor}
  @argument[result]{a @symbol{graphene:matrix-t} instance}
  @return{The @symbol{graphene:matrix-t} instance with the interpolated matrix.}
  @begin{short}
    Linearly interpolates the two given matrices by interpolating the decomposed
    transformations separately.
  @end{short}
  If either matrix cannot be reduced to their transformations then the
  interpolation cannot be performed, and this function will return an identity
  matrix.
  @see-symbol{graphene:matrix-t}"
  (cffi:foreign-funcall "graphene_matrix_interpolate"
                        (:pointer (:struct matrix-t)) a
                        (:pointer (:struct matrix-t)) b
                        :double (coerce factor 'double-float)
                        (:pointer (:struct matrix-t)) result
                        :void)
  result)

(export 'matrix-interpolate)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_equal ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_matrix_equal" matrix-equal) :bool
 #+liber-documentation
 "@version{#2022-9-30}
  @argument[a]{a @symbol{graphene:matrix-t} instance}
  @argument[b]{a @symbol{graphene:vec3-t} instance}
  @return{@em{True} if the two matrices are equal, @em{false} otherwise.}
  @short{Checks whether the two given matrices are equal.}
  @see-symbol{graphene:matrix-t}"
  (a (:pointer (:struct matrix-t)))
  (b (:pointer (:struct matrix-t))))

(export 'matrix-equal)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_equal_fast ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_matrix_equal_fast" matrix-equal-fast) :bool
 #+liber-documentation
 "@version{#2022-9-30}
  @argument[a]{a @symbol{graphene:matrix-t} instance}
  @argument[b]{a @symbol{graphene:vec3-t} instance}
  @return{@em{True} if the two matrices are equal, @em{false} otherwise.}
  @begin{short}
    Checks whether the two given matrices are byte-by-byte equal.
  @end{short}

  While this function is faster than the @fun{graphene:matrix-equal} function,
  it can also return false negatives, so it should be used in conjuction with
  either the @fun{graphene:matrix-equal} or @fun{graphene:matrix-near} function.
  For instance:
  @begin{pre}
if (graphene_matrix_equal_fast (a, b))
  {
    // matrices are definitely the same
  @}
else
  {
    if (graphene_matrix_equal (a, b))
      // matrices contain the same values within an epsilon of FLT_EPSILON
    else if (graphene_matrix_near (a, b, 0.0001))
      // matrices contain the same values within an epsilon of 0.0001
    else
      // matrices are not equal
  @}
  @end{pre}
  @see-symbol{graphene:matrix-t}"
  (a (:pointer (:struct matrix-t)))
  (b (:pointer (:struct matrix-t))))

(export 'matrix-equal-fast)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_near ()
;;; ----------------------------------------------------------------------------

(defun matrix-near (a b epsilon)
 #+liber-documentation
 "@version{#2022-9-30}
  @argument[a]{a @symbol{graphene:matrix-t} instance}
  @argument[b]{a @symbol{graphene:vec3-t} instance}
  @argument[epsilon]{a float with the threshold between the two matrices, and
    @em{false} otherwise}
  @return{@em{True} if the two matrices are near each other, @em{false}
    otherwise.}
  @begin{short}
    Compares the two given matrices and checks whether their values are within
    the given epsilon of each other.
  @end{short}
  @see-symbol{graphene:matrix-t}"
  (cffi:foreign-funcall "graphene_matrix_near"
                        (:pointer (:struct matrix-t)) a
                        (:pointer (:struct matrix-t)) b
                        :float (coerce epsilon 'single-float)
                        :bool))

(export 'matrix-near)

;;; ----------------------------------------------------------------------------
;;; graphene_matrix_print ()
;;;
;;; void
;;; graphene_matrix_print (const graphene_matrix_t *m);
;;;
;;; Prints the contents of a matrix to the standard error stream.
;;;
;;; This function is only useful for debugging; there are no guarantees made on
;;; the format of the output.
;;;
;;; m :
;;;     The matrix to print
;;; ----------------------------------------------------------------------------

;;; --- End of file graphene.matrix.lisp ---------------------------------------
