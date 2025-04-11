;;; ----------------------------------------------------------------------------
;;; graphene.box.lisp
;;;
;;; The documentation in this file is taken from the GRAPHENE Reference Manual
;;; and modified to document the Lisp binding to the Graphene library, see
;;; <https://ebassi.github.io/graphene/docs/>. The API documentation for the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 - 2025 Dieter Kaiser
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
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;; ----------------------------------------------------------------------------
;;;
;;; Box
;;;
;;;     Axis-aligned bounding box
;;;
;;; Types and Values
;;;
;;;     graphene_box_t
;;;
;;; Functions
;;;
;;;     graphene_box_alloc
;;;     graphene_box_free
;;;     graphene_box_init
;;;     graphene_box_init_from_box
;;;     graphene_box_init_from_points                       not implemented
;;;     graphene_box_init_from_vec3
;;;     graphene_box_init_from_vectors                      not implemented
;;;     graphene_box_equal
;;;     graphene_box_expand
;;;     graphene_box_expand_scalar
;;;     graphene_box_expand_vec3
;;;     graphene_box_get_min
;;;     graphene_box_get_max
;;;     graphene_box_get_center
;;;     graphene_box_get_depth
;;;     graphene_box_get_height
;;;     graphene_box_get_width
;;;     graphene_box_get_size
;;;     graphene_box_get_bounding_sphere
;;;     graphene_box_get_vertices
;;;     graphene_box_union
;;;     graphene_box_intersection
;;;     graphene_box_contains_box
;;;     graphene_box_contains_point
;;;     graphene_box_zero
;;;     graphene_box_one
;;;     graphene_box_minus_one
;;;     graphene_box_one_minus_one
;;;     graphene_box_empty
;;;     graphene_box_infinite
;;; ----------------------------------------------------------------------------

(in-package :graphene)

;;; ----------------------------------------------------------------------------
;;; graphene:with-box
;;; ----------------------------------------------------------------------------

(defmacro with-box ((var &rest args) &body body)
 #+liber-documentation
 "@version{2025-4-5}
  @syntax{(graphene:with-box (box) body) => result}
  @syntax{(graphene:with-box (box box1) body) => result}
  @syntax{(graphene:with-box (box pmin pmax) body) => result}
  @syntax{(graphene:with-box (box (vmin graphene:vec3-t) vmax) body) => result}
  @argument[box]{a @symbol{graphene:box-t} instance to create and initialize}
  @argument[box1]{a @symbol{graphene:box-t} instance to use for initialization}
  @argument[pmin]{a @symbol{graphene:point3d-t} instance to use for
    initialization}
  @argument[pmax]{a @symbol{graphene:point3d-t} instance to use for
    initialization}
  @argument[vmin]{a @symbol{graphene:vec3-t} instance to use for initialization}
  @argument[vmax]{a @symbol{graphene:vec3-t} instance to use for initialization}
  @begin{short}
    The @fun{graphene:with-box} macro allocates a new @symbol{graphene:box-t}
    instance, initializes the box with the given values and executes the body
    that uses the box.
  @end{short}
  After execution of the body the allocated memory for the box is released.

  When no argument is given the components of the box are initialized to zero.
  The initialization with two points uses the @fun{graphene:box-init} function.
  If the first value has the @code{graphene:vec3-t} type the
  @fun{graphene:box-init-from-vec3} function is used for initialization with
  two vectors. The initialization from another box is done with the
  @fun{graphene:box-init-from-box} function.
  @begin[Notes]{dictionary}
    The memory is allocated with the @fun{graphene:box-alloc} function and
    released with the @fun{graphene:box-free} function.
  @end{dictionary}
  @see-symbol{graphene:box-t}
  @see-symbol{graphene:point3d-t}
  @see-symbol{graphene:vec3-t}
  @see-macro{graphene:with-boxes}
  @see-function{graphene:box-alloc}
  @see-function{graphene:box-free}"
  (cond ((null args)
         ;; No arguments, the default is initialization with zeros
         `(let ((,var (box-alloc)))
            (box-init-from-box ,var (box-zero))
            (unwind-protect
              (progn ,@body)
              (box-free ,var))))
        ((null (second args))
         ;; One argument, the argument must be of type box-t
         (dbind (arg1 &optional type1 &rest rest1) (mklist (first args))
           (declare (ignore rest1))
           (cond ((eq type1 'box-t)
                  ;; One argument of type box-t
                  `(let ((,var (box-alloc)))
                     (box-init-from-box ,var ,arg1)
                     (unwind-protect
                       (progn ,@body)
                       (box-free ,var))))
                 (t
                  ;; One argument with no type, default is box-t
                  `(let ((,var (box-alloc)))
                     (box-init-from-box ,var ,@args)
                     (unwind-protect
                       (progn ,@body)
                       (box-free ,var)))))))
        ((null (third args))
         ;; Two arguments, the first can be of type point3d-t or vec3-t
         (dbind (arg1 &optional type1 &rest rest1) (mklist (first args))
           (declare (ignore rest1))
           (cond ((eq type1 'point3d-t)
                  ;; First argument of type point3d-t
                  `(let ((,var (box-alloc)))
                     (box-init ,var ,arg1 ,@(rest args))
                     (unwind-protect
                       (progn ,@body)
                       (box-free ,var))))
                 ((eq type1 'vec3-t)
                  ;; First argument of type vec3-t
                  `(let ((,var (box-alloc)))
                     (box-init-from-vec3 ,var ,arg1 ,@(rest args))
                     (unwind-protect
                       (progn ,@body)
                       (box-free ,var))))
                 (t
                  `(let ((,var (box-alloc)))
                     (box-init ,var ,@args)
                     (unwind-protect
                       (progn ,@body)
                       (box-free ,var)))))))
        (t
         (error "Syntax error in GRAPHENE:WITH-BOX"))))

(export 'with-box)

;;; ----------------------------------------------------------------------------
;;; graphene:with-boxes
;;; ----------------------------------------------------------------------------

(defmacro with-boxes (vars &body body)
 #+liber-documentation
 "@version{2025-4-5}
  @syntax{(graphene:with-boxes (box1 box2 ... boxn) body) => result}
  @argument[box1 ... boxn]{newly created @symbol{graphene:box-t} instances}
  @argument[body]{a body that uses the bindings @arg{box1 ... boxn}}
  @begin{short}
    The @fun{graphene:with-boxes} macro creates new variable bindings and
    executes the body that use these bindings.
  @end{short}
  The macro performs the bindings sequentially, like the @sym{let*} macro.

  Each box can be initialized with values using the syntax for the
  @fun{graphene:with-box} macro. See also the @fun{graphene:with-box}
  documentation.
  @see-symbol{graphene:box-t}
  @see-macro{graphene:with-box}"
  (if vars
      (let ((var (mklist (first vars))))
        `(with-box ,var
           (with-boxes ,(rest vars)
             ,@body)))
      `(progn ,@body)))

(export 'with-boxes)

;;; ----------------------------------------------------------------------------
;;; graphene_box_t
;;; ----------------------------------------------------------------------------

(cffi:defcstruct box-t)

#+liber-documentation
(setf (liber:alias-for-symbol 'box-t)
      "CStruct"
      (liber:symbol-documentation 'box-t)
 "@version{2025-4-5}
  @begin{declaration}
(cffi:defcstruct box-t)
  @end{declaration}
  @begin{short}
    The @symbol{graphene:box-t} structure provides a representation of an axis
    aligned minimum bounding box using the coordinates of its minimum and
    maximum vertices.
  @end{short}")

(export 'box-t)

;;; ----------------------------------------------------------------------------
;;; graphene_box_alloc
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_box_alloc" box-alloc) (:pointer (:struct box-t))
 #+liber-documentation
 "@version{2025-4-5}
  @return{The newly allocated @symbol{graphene:box-t} instance.}
  @begin{short}
    Allocates a new @symbol{graphene:box-t} instance.
  @end{short}
  The contents of the returned instance are undefined. Use the
  @fun{graphene:box-free} function to free the resources allocated by this
  function.
  @see-symbol{graphene:box-t}
  @see-function{graphene:box-free}")

(export 'box-alloc)

;;; ----------------------------------------------------------------------------
;;; graphene_box_free
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_box_free" box-free) :void
 #+liber-documentation
 "@version{2025-4-5}
  @argument[box]{a @symbol{graphene:box-t} instance}
  @begin{short}
    Frees the resources allocated by the @fun{graphene:box-alloc} function.
  @end{short}
  @see-symbol{graphene:box-t}
  @see-function{graphene:box-alloc}"
  (box (:pointer (:struct box-t))))

(export 'box-free)

;;; ----------------------------------------------------------------------------
;;; graphene_box_init
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_box_init" box-init) (:pointer (:struct box-t))
 #+liber-documentation
 "@version{2025-4-5}
  @argument[box]{a @symbol{graphene:box-t} instance to initialize}
  @argument[pmin]{a @symbol{graphene:point3d-t} instance for the coordinates
    of the minimum vertex}
  @argument[pmax]{a @symbol{graphene:point3d-t} instance for the coordinates
    of the maximum vertex}
  @return{The initialized @symbol{graphene:box-t} instance.}
  @begin{short}
    Initializes the given @symbol{graphene:box-t} instance with two vertices.
  @end{short}
  @see-symbol{graphene:box-t}
  @see-symbol{graphene:point3d-t}"
  (box (:pointer (:struct box-t)))
  (pmin (:pointer (:struct point3d-t)))
  (pmax (:pointer (:struct point3d-t))))

(export 'box-init)

;;; ----------------------------------------------------------------------------
;;; graphene_box_init_from_box
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_box_init_from_box" box-init-from-box)
    (:pointer (:struct box-t))
 #+liber-documentation
 "@version{2025-4-5}
  @argument[box]{a @symbol{graphene:box-t} instance to initialize}
  @argument[source]{a @symbol{graphene:box-t} instance}
  @return{The initialized @symbol{graphene:box-t} instance.}
  @begin{short}
    Initializes the given @symbol{graphene:box-t} instance with the vertices of
    another @symbol{graphene:box-t} instance.
  @end{short}
  @see-symbol{graphene:box-t}"
  (box (:pointer (:struct box-t)))
  (source (:pointer (:struct box-t))))

(export 'box-init-from-box)

;;; ----------------------------------------------------------------------------
;;; graphene_box_init_from_points                           not implemented
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; graphene_box_init_from_vec3
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_box_init_from_vec3" box-init-from-vec3)
    (:pointer (:struct box-t))
 #+liber-documentation
 "@version{2025-4-5}
  @argument[box]{a @symbol{graphene:box-t} instance to initialize}
  @argument[vmin]{a @symbol{graphene:vec3-t} instance for the coordinates of
    the minimum vertex}
  @argument[vmax]{a @symbol{graphene:vec3-t} instance for the coordinates of
    the maximum vertex}
  @return{The initialized @symbol{graphene:box-t} instance.}
  @begin{short}
    Initializes the given @symbol{graphene:box-t} instance with two vertices.
  @end{short}
  @see-symbol{graphene:box-t}
  @see-symbol{graphene:vec3-t}"
  (box (:pointer (:struct box-t)))
  (vmin (:pointer (:struct vec3-t)))
  (vmax (:pointer (:struct vec3-t))))

(export 'box-init-from-vec3)

;;; ----------------------------------------------------------------------------
;;; graphene_box_init_from_vectors                          not implemented
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; graphene_box_equal
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_box_equal" box-equal) :bool
 #+liber-documentation
 "@version{2025-4-5}
  @argument[a]{a @symbol{graphene:box-t} instance}
  @argument[b]{a @symbol{graphene:box-t} instance}
  @return{@em{True} if the boxes are equal, otherwise @em{false}}
  @begin{short}
    Checks whether the two given boxes are equal.
  @end{short}
  @see-symbol{graphene:box-t}"
  (a (:pointer (:struct box-t)))
  (b (:pointer (:struct box-t))))

(export 'box-equal)

;;; ----------------------------------------------------------------------------
;;; graphene_box_expand
;;; ----------------------------------------------------------------------------

(defun box-expand (box point result)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[box]{a @symbol{graphene:box-t} instance to expand}
  @argument[point]{a @symbol{graphene:point3d-t} instance for the coordinates
    of the point to include}
  @argument[result]{a @symbol{graphene:box-t} instance for the result}
  @return{The @symbol{graphene:box-t} instance with the result.}
  @begin{short}
    Expands the dimensions of @arg{box} to include the coordinates at
    @arg{point}.
  @end{short}
  @see-symbol{graphene:box-t}
  @see-symbol{graphene:point3d-t}"
  (cffi:foreign-funcall "graphene_box_expand"
                        (:pointer (:struct box-t)) box
                        (:pointer (:struct point3d-t)) point
                        (:pointer (:struct box-t)) result
                        :void)
  result)

(export 'box-expand)

;;; ----------------------------------------------------------------------------
;;; graphene_box_expand_scalar
;;; ----------------------------------------------------------------------------

(defun box-expand-scalar (box scalar result)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[box]{a @symbol{graphene:box-t} instance to expand}
  @argument[scalar]{a number coerced to a single float for the scalar value}
  @argument[result]{a @symbol{graphene:box-t} instance for the result}
  @return{The @symbol{graphene:box-t} instance with the result.}
  @begin{short}
    Expands the dimensions of @arg{box} by the given scalar value.
  @end{short}
  If the @arg{scalar} argument is positive, the box will grow. If the
  @arg{scalar} argument is negative, the box will shrink.
  @see-symbol{graphene:box-t}"
  (cffi:foreign-funcall "graphene_box_expand_scalar"
                        (:pointer (:struct box-t)) box
                        :float (coerce scalar 'single-float)
                        (:pointer (:struct box-t)) result
                        :void)
  result)

(export 'box-expand-scalar)

;;; ----------------------------------------------------------------------------
;;; graphene_box_expand_vec3
;;; ----------------------------------------------------------------------------

(defun box-expand-vec3 (box vector result)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[box]{a @symbol{graphene:box-t} instance to expand}
  @argument[vector]{a @symbol{graphene:vec3-t} instance for the coordinates of
    the vector to include}
  @argument[result]{a @symbol{graphene:box-t} instance for the result}
  @return{The @symbol{graphene:box-t} instance with the result.}
  @begin{short}
    Expands the dimensions of @arg{box} to include the coordinates of the
    given vector.
  @end{short}
  @see-symbol{graphene:box-t}
  @see-symbol{graphene:vec3-t}"
  (cffi:foreign-funcall "graphene_box_expand_vec3"
                        (:pointer (:struct box-t)) box
                        (:pointer (:struct vec3-t)) vector
                        (:pointer (:struct box-t)) result
                        :void)
  result)

(export 'box-expand-vec3)

;;; ----------------------------------------------------------------------------
;;; graphene_box_get_min
;;; ----------------------------------------------------------------------------

(defun box-min (box pmin)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[box]{a @symbol{graphene:box-t} instance}
  @argument[pmin]{a @symbol{graphene:point3d-t} instance for the minimum point}
  @return{The @symbol{graphene:point3d-t} instance with the result.}
  @begin{short}
    Retrieves the coordinates of the minimum point of the given box.
  @end{short}
  @see-symbol{graphene:box-t}
  @see-symbol{graphene:point3d-t}"
  (cffi:foreign-funcall "graphene_box_get_min"
                        (:pointer (:struct box-t)) box
                        (:pointer (:struct point3d-t)) pmin
                        :void)
  pmin)

(export 'box-min)

;;; ----------------------------------------------------------------------------
;;; graphene_box_get_max
;;; ----------------------------------------------------------------------------

(defun box-max (box pmax)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[box]{a @symbol{graphene:box-t} instance}
  @argument[pmax]{a @symbol{graphene:point3d-t} instance for the maximum point}
  @return{The @symbol{graphene:point3d-t} instance with the result.}
  @begin{short}
    Retrieves the coordinates of the maximum point of the given box.
  @end{short}
  @see-symbol{graphene:box-t}
  @see-symbol{graphene:point3d-t}"
  (cffi:foreign-funcall "graphene_box_get_max"
                        (:pointer (:struct box-t)) box
                        (:pointer (:struct point3d-t)) pmax
                        :void)
  pmax)

(export 'box-max)

;;; ----------------------------------------------------------------------------
;;; graphene_box_get_center
;;; ----------------------------------------------------------------------------

(defun box-center (box center)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[box]{a @symbol{graphene:box-t} instance}
  @argument[center]{a @symbol{graphene:point3d-t} instance for the coordinates
    of the center}
  @return{The @symbol{graphene:point3d-t} instance with the result.}
  @begin{short}
    Retrieves the coordinates of the center of the box.
  @end{short}
  @see-symbol{graphene:box-t}
  @see-symbol{graphene:point3d-t}"
  (cffi:foreign-funcall "graphene_box_get_center"
                        (:pointer (:struct box-t)) box
                        (:pointer (:struct point3d-t)) center
                        :void)
  center)

(export 'box-center)

;;; ----------------------------------------------------------------------------
;;; graphene_box_get_depth
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_box_get_depth" box-depth) :float
 #+liber-documentation
 "@version{2025-4-5}
  @argument[box]{a @symbol{graphene:box-t} instance}
  @return{The single float with the depth of the box.}
  @begin{short}
    Retrieves the size of the box on the Z axis.
  @end{short}
  @see-symbol{graphene:box-t}"
  (box (:pointer (:struct box-t))))

(export 'box-depth)

;;; ----------------------------------------------------------------------------
;;; graphene_box_get_height
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_box_get_height" box-height) :float
 #+liber-documentation
 "@version{2025-4-5}
  @argument[box]{a @symbol{graphene:box-t} instance}
  @return{The single float with the height of the box.}
  @begin{short}
    Retrieves the size of the box on the Y axis.
  @end{short}
  @see-symbol{graphene:box-t}"
  (box (:pointer (:struct box-t))))

(export 'box-height)

;;; ----------------------------------------------------------------------------
;;; graphene_box_get_width
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_box_get_width" box-width) :float
 #+liber-documentation
 "@version{2025-4-5}
  @argument[box]{a @symbol{graphene:box-t} instance}
  @return{The single float with the width of the box.}
  @begin{short}
    Retrieves the size of the box on the X axis.
  @end{short}
  @see-symbol{graphene:box-t}"
  (box (:pointer (:struct box-t))))

(export 'box-width)

;;; ----------------------------------------------------------------------------
;;; graphene_box_get_size
;;; ----------------------------------------------------------------------------

(defun box-size (box size)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[box]{a @symbol{graphene:box-t} instance}
  @argument[size]{a @symbol{graphene:vec3-t} instance for the size}
  @return{The @symbol{graphene:vec3-t} instance with the size.}
  @begin{short}
    Retrieves the size of the box on all three axes, and stores it into the
    given size vector.
  @end{short}
  @see-symbol{graphene:box-t}
  @see-symbol{graphene:vec3-t}"
  (cffi:foreign-funcall "graphene_box_get_size"
                        (:pointer (:struct box-t)) box
                        (:pointer (:struct vec3-t)) size
                        :void)
  size)

(export 'box-size)

;;; ----------------------------------------------------------------------------
;;; graphene_box_get_bounding_sphere
;;; ----------------------------------------------------------------------------

(defun box-bounding-sphere (box sphere)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[box]{a @symbol{graphene:box-t} instance}
  @argument[sphere]{a @symbol{graphene:sphere-t} instance for the bounding
    sphere}
  @return{The @symbol{graphene:sphere-t} instance with the bounding sphere.}
  @begin{short}
    Computes the bounding sphere capable of containing the given box.
  @end{short}
  @see-symbol{graphene:box-t}
  @see-symbol{graphene:sphere-t}"
  (cffi:foreign-funcall "graphene_box_get_bounding_sphere"
                        (:pointer (:struct box-t)) box
                        :pointer sphere ; for (:struct sphere-t)
                        :void)
  sphere)

(export 'box-bounding-sphere)

;;; ----------------------------------------------------------------------------
;;; graphene_box_get_vertices
;;; ----------------------------------------------------------------------------

(defun box-vertices (box vertices)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[box]{a @symbol{graphene:box-t} instance}
  @argument[vertices]{a list of eigth @symbol{graphene:vec3-t} instances}
  @begin{return}
    The list of eight @symbol{graphene:vec3-t} instances with the vertices.
  @end{return}
  @begin{short}
    Computes the vertices of the given box.
  @end{short}
  @begin[Examples]{dictionary}
    @begin{pre}
(graphene:with-vec3s (v0 v1 v2 v3 v4 v5 v6 v7)
  (graphene:with-point3ds ((min 0 0 0) (max 1 1 1))
    (graphene:with-box (box min max)
      (mapcar #'vec3-to-float
              (box-vertices box (list v0 v1 v2 v3 v4 v5 v6 v7))))))
=> ((0.0 0.0 0.0) (0.0 0.0 1.0) (0.0 1.0 0.0) (0.0 1.0 1.0)
    (1.0 0.0 0.0) (1.0 0.0 1.0) (1.0 1.0 0.0) (1.0 1.0 1.0))
    @end{pre}
  @end{dictionary}
  @see-symbol{graphene:box-t}
  @see-symbol{graphene:vec3-t}"
  (with-point3ds (pmin pmax)
    (box-min box pmin)
    (box-max box pmax)
    (let ((v0 (elt vertices 0))
          (v1 (elt vertices 1))
          (v2 (elt vertices 2))
          (v3 (elt vertices 3))
          (v4 (elt vertices 4))
          (v5 (elt vertices 5))
          (v6 (elt vertices 6))
          (v7 (elt vertices 7)))
      (vec3-init v0 (point3d-x pmin) (point3d-y pmin) (point3d-z pmin))
      (vec3-init v1 (point3d-x pmin) (point3d-y pmin) (point3d-z pmax))
      (vec3-init v2 (point3d-x pmin) (point3d-y pmax) (point3d-z pmin))
      (vec3-init v3 (point3d-x pmin) (point3d-y pmax) (point3d-z pmax))
      (vec3-init v4 (point3d-x pmax) (point3d-y pmin) (point3d-z pmin))
      (vec3-init v5 (point3d-x pmax) (point3d-y pmin) (point3d-z pmax))
      (vec3-init v6 (point3d-x pmax) (point3d-y pmax) (point3d-z pmin))
      (vec3-init v7 (point3d-x pmax) (point3d-y pmax) (point3d-z pmax))
      (list v0 v1 v2 v3 v4 v5 v6 v7))))

(export 'box-vertices)

;;; ----------------------------------------------------------------------------
;;; graphene_box_union
;;; ----------------------------------------------------------------------------

(defun box-union (a b result)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[a]{a @symbol{graphene:box-t} instance}
  @argument[b]{a @symbol{graphene:box-t} instance}
  @argument[result]{a @symbol{graphene:box-t} instance for the result}
  @return{The @symbol{graphene:box-t} instance with the result.}
  @begin{short}
    Unions the two given boxes.
  @end{short}
  @see-symbol{graphene:box-t}"
  (cffi:foreign-funcall "graphene_box_union"
                        (:pointer (:struct box-t)) a
                        (:pointer (:struct box-t)) b
                        (:pointer (:struct box-t)) result
                        :void)
  result)

(export 'box-union)

;;; ----------------------------------------------------------------------------
;;; graphene_box_intersection
;;; ----------------------------------------------------------------------------

;; TODO: Check the implementation, should we return only one value,
;; which could be a valid box or NIL

(defun box-intersection (a b result)
 #+liber-documentation
 "@version{2025-4-5}
  @syntax{(graphene:box-intersection a b result) => result, success}
  @argument[a]{a @symbol{graphene:box-t} instance}
  @argument[b]{a @symbol{graphene:box-t} instance}
  @argument[result]{a @symbol{graphene:box-t} instance for the result}
  @argument[success]{@em{true} if the two boxes intersect}
  @begin{short}
    Intersects the two given boxes.
  @end{short}
  If the two boxes do not intersect, @arg{result} will contain a degenerate box
  initialized with the @fun{graphene:box-empty} values.
  @see-symbol{graphene:box-t}
  @see-function{graphene:box-empty}"
  (let ((success (cffi:foreign-funcall "graphene_box_intersection"
                                       (:pointer (:struct box-t)) a
                                       (:pointer (:struct box-t)) b
                                       (:pointer (:struct box-t)) result
                                       :bool)))
    (values result success)))

(export 'box-intersection)

;;; ----------------------------------------------------------------------------
;;; graphene_box_contains_box
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_box_contains_box" box-contains-box) :bool
 #+liber-documentation
 "@version{2025-4-5}
  @argument[a]{a @symbol{graphene:box-t} instance}
  @argument[b]{a @symbol{graphene:box-t} instance}
  @return{@em{True} if @arg{b} is contained in @arg{a}.}
  @begin{short}
    Checks whether the the box @arg{a} contains the given box @arg{b}.
  @end{short}
  @see-symbol{graphene:box-t}"
  (a (:pointer (:struct box-t)))
  (b (:pointer (:struct box-t))))

(export 'box-contains-box)

;;; ----------------------------------------------------------------------------
;;; graphene_box_contains_point
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_box_contains_point" box-contains-point) :bool
 #+liber-documentation
 "@version{2025-4-5}
  @argument[box]{a @symbol{graphene:box-t} instance}
  @argument[point]{a @symbol{graphene:point3d-t} instance for the coordinates
    to check}
  @return{@em{True} if the point is contained in the box.}
  @begin{short}
    Checks whether the box contains the given point.
  @end{short}
  @see-symbol{graphene:box-t}
  @see-symbol{graphene:point3d-t}"
  (a (:pointer (:struct box-t)))
  (b (:pointer (:struct point3d-t))))

(export 'box-contains-point)

;;; ----------------------------------------------------------------------------
;;; graphene_box_zero
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_box_zero" box-zero) (:pointer (:struct box-t))
 #+liber-documentation
 "@version{2025-4-5}
  @return{The @symbol{graphene:box-t} instance.}
  @begin{short}
    Returns a box with both the minimum and maximum vertices set at (0, 0, 0).
  @end{short}
  The returned box is owned by Graphene and should not be modified or freed.
  @see-symbol{graphene:box-t}")

(export 'box-zero)

;;; ----------------------------------------------------------------------------
;;; graphene_box_one
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_box_one" box-one) (:pointer (:struct box-t))
 #+liber-documentation
 "@version{2025-4-5}
  @return{The @symbol{graphene:box-t} instance.}
  @begin{short}
    Returns a box with both the minimum and maximum vertices set at (1, 1, 1).
  @end{short}
  The returned box is owned by Graphene and should not be modified or freed.
  @see-symbol{graphene:box-t}")

(export 'box-one)

;;; ----------------------------------------------------------------------------
;;; graphene_box_minus_one
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_box_minus_one" box-minus-one)
    (:pointer (:struct box-t))
 #+liber-documentation
 "@version{2025-4-5}
  @return{The @symbol{graphene:box-t} instance.}
  @begin{short}
    Returns a box with the minimum vertex set at (-1, -1, -1) and the maximum
    vertex set at (0, 0, 0).
  @end{short}
  The returned box is owned by Graphene and should not be modified or freed.
  @see-symbol{graphene:box-t}")

(export 'box-minus-one)

;;; ----------------------------------------------------------------------------
;;; graphene_box_one_minus_one
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_box_one_minus_one" box-one-minus-one)
    (:pointer (:struct box-t))
 #+liber-documentation
 "@version{2025-4-5}
  @return{The @symbol{graphene:box-t} instance.}
  @begin{short}
    Returns a box the minimum vertex set at (-1, -1, -1) and the maximum
    vertex set at (1, 1, 1).
  @end{short}
  The returned box is owned by Graphene and should not be modified or freed.
  @see-symbol{graphene:box-t}")

(export 'box-one-minus-one)

;;; ----------------------------------------------------------------------------
;;; graphene_box_empty
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_box_empty" box-empty) (:pointer (:struct box-t))
 #+liber-documentation
 "@version{2025-4-5}
  @return{The @symbol{graphene:box-t} instance.}
  @begin{short}
    Returns a degenerate box that can only be expanded.
  @end{short}
  The returned box is owned by Graphene and should not be modified or freed.
  @see-symbol{graphene:box-t}")

(export 'box-empty)

;;; ----------------------------------------------------------------------------
;;; graphene_box_infinite
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_box_infinite" box-infinite) (:pointer (:struct box-t))
 #+liber-documentation
 "@version{2025-4-5}
  @return{The @symbol{graphene:box-t} instance.}
  @begin{short}
    Returns a degenerate box that cannot be expanded.
  @end{short}
  The returned box is owned by Graphene and should not be modified or freed.
  @see-symbol{graphene:box-t}")

(export 'box-infinite)

;;; --- End of file graphene.box.lisp ------------------------------------------
