;;; ----------------------------------------------------------------------------
;;; graphene.box.lisp
;;;
;;; The documentation of this file is taken from the GRAPHENE Reference Manual
;;; and modified to document the Lisp binding to the Graphene library.
;;; See <https://ebassi.github.io/graphene/docs/>.
;;; The API documentation of the Lisp binding is available from
;;; <http://www.crategus.com/books/cl-cffi-graphene/>.
;;;
;;; Copyright (C) 2022 Dieter Kaiser
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License for Lisp
;;; as published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version and with a preamble to
;;; the GNU Lesser General Public License that clarifies the terms for use
;;; with Lisp programs and is referred as the LLGPL.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program and the preamble to the Gnu Lesser
;;; General Public License.  If not, see <http://www.gnu.org/licenses/>
;;; and <http://opensource.franz.com/preamble.html>.
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
;;;     graphene_box_init_from_points
;;;     graphene_box_init_from_vec3
;;;     graphene_box_init_from_vectors
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

(defmacro with-graphene-box ((var &rest args) &body body)
  (cond ((not args)
         ;; No arguments, the default is initialization with zeros.
         `(let ((,var (box-alloc)))
            (box-init-from-box ,var (box-zero))
            (unwind-protect
              (progn ,@body)
              (box-free ,var))))
        ((not (second args))
         ;; One argument, the argument must be of type box-t.
         (destructuring-bind (arg &optional type)
             (if (listp (first args)) (first args) (list (first args)))
           (cond ((or (not type)
                      (eq type 'box-t))
                  ;; One argument with no type or of type box-t
                  `(let ((,var (box-alloc)))
                     (box-init-from-box ,var ,arg)
                     (unwind-protect
                       (progn ,@body)
                       (box-free ,var))))
                 (t
                  (error "Type error in WITH-GRAPHENE-BOX")))))
        ((not (third args))
         ;; Two arguments, the first can be of type point3d-t or vec3-t.
         ;; TODO: Combine the two destructuring-bind calls to onw call.
         (destructuring-bind (arg1 &optional type1)
             (if (listp (first args)) (first args) (list (first args)))
           (destructuring-bind (arg2 &optional type2)
               (if (listp (second args)) (second args) (list (second args)))
             (cond ((and (or (not type1)
                             (eq type1 'point3d-t))
                         (or (not type2)
                             (eq type2 'point3d-t)))
                    ;; First argument with no type or of type point3d-t and
                    ;; second argument with no type or type point3d-t
                    `(let ((,var (box-alloc)))
                       (box-init ,var ,arg1 ,arg2)
                       (unwind-protect
                         (progn ,@body)
                         (box-free ,var))))
                   ((and (eq type1 'vec3-t)
                         (or (not type2)
                             (eq type2 'vec3-t)))
                    ;; First argument of type vec3-t and second argument with
                    ;; no type or type vec3-t
                    `(let ((,var (box-alloc)))
                       (box-init-from-vec3 ,var ,arg1 ,arg2)
                       (unwind-protect
                         (progn ,@body)
                         (box-free ,var))))
                   (t
                    (error "Type error in WITH-GRAPHENE-BOX"))))))
        (t
         (error "Syntax error in WITH-GRAPHENE-BOX"))))

(export 'with-graphene-box)

(defmacro with-graphene-boxes (vars &body body)
  (if vars
      (let ((var (if (listp (first vars)) (first vars) (list (first vars)))))
        `(with-graphene-box ,var
           (with-graphene-boxes ,(rest vars)
             ,@body)))
      `(progn ,@body)))

(export 'with-graphene-boxes)

;;; ----------------------------------------------------------------------------
;;; graphene_box_t
;;; ----------------------------------------------------------------------------

(defcstruct box-t)

#+liber-documentation
(setf (liber:alias-for-symbol 'box-t)
      "CStruct"
      (liber:symbol-documentation 'box-t)
 "@version{#2022-9-22}
  @begin{short}
    The @sym{box-t} structure provides a representation of an axis aligned
    minimum bounding box using the coordinates of its minimum and maximum
    vertices.
  @end{short}")

(export 'box-t)

;;; ----------------------------------------------------------------------------
;;; graphene_box_alloc ()
;;; ----------------------------------------------------------------------------

(defcfun ("graphene_box_alloc" box-alloc)
    (:pointer (:struct box-t))
 #+liber-documentation
 "@version{#2022-9-22}
  @return{The newly allocated @symbol{box-t} instance. Use the
    @fun{box-free} function to free the resources allocated by this function.}
  @begin{short}
    Allocates a new @symbol{box-t} instance.
  @end{short}
  The contents of the returned instance are undefined.
  @see-symbol{box-t}
  @see-function{box-free}")

(export 'box-alloc)

;;; ----------------------------------------------------------------------------
;;; graphene_box_free ()
;;; ----------------------------------------------------------------------------

(defcfun ("graphene_box_free" box-free) :void
 #+liber-documentation
 "@version{#2022-9-22}
  @argument[box]{a @symbol{box-t} instance}
  @begin{short}
    Frees the resources allocated by the @fun{box-alloc} function.
  @end{short}
  @see-symbol{box-t}
  @see-function{box-alloc}"
  (box (:pointer (:struct box-t))))

(export 'box-free)

;;; ----------------------------------------------------------------------------
;;; graphene_box_init ()
;;; ----------------------------------------------------------------------------

(defcfun ("graphene_box_init" box-init) (:pointer (:struct box-t))
 #+liber-documentation
 "@version{#2022-9-22}
  @argument[box]{a @symbol{box-t} instance to initialize}
  @argument[min]{a @symbol{point3d-t} instance with the coordinates of the
    minimum vertex}
  @argument[max]{a @symbol{point3d-t} instance with the coordinates of the
    maximum vertex}
  @return{The initialized @symbol{box-t} instance.}
  @begin{short}
    Initializes the given @symbol{box-t} instance with two vertices.
  @end{short}
  @see-symbol{box-t}
  @see-symbol{point3d-t}"
  (box (:pointer (:struct box-t)))
  (min (:pointer (:struct point3d-t)))
  (max (:pointer (:struct point3d-t))))

(export 'box-init)

;;; ----------------------------------------------------------------------------
;;; graphene_box_init_from_box ()
;;; ----------------------------------------------------------------------------

(defcfun ("graphene_box_init_from_box" box-init-from-box)
    (:pointer (:struct box-t))
 #+liber-documentation
 "@version{#2022-9-22}
  @argument[box]{a @symbol{box-t} instance to initialize}
  @argument[source]{a @symbol{box-t} instance}
  @return{The initialized @symbol{box-t} instance.}
  @begin{short}
    Initializes the given @symbol{box-t} instance with the vertices of
    another @symbol{box-t} instance.
  @end{short}
  @see-symbol{box-t}"
  (box (:pointer (:struct box-t)))
  (source (:pointer (:struct box-t))))

(export 'box-init-from-box)

;;; ----------------------------------------------------------------------------
;;; graphene_box_init_from_points ()
;;; ----------------------------------------------------------------------------

(defun box-init-from-points (box arg)
 #+liber-documentation
 "@version{#2022-9-22}
  @argument[box]{a @symbol{box-t} instance to initialize}
  @argument[arg]{a list with two @symbol{point3d-t} instances}
  @return{The initialized @symbol{box-t} instance.}
  @begin{short}
    Initializes the given @symbol{box-t} instance with the vertices of
    the given @symbol{point3d-t} instances.
  @end{short}
  @see-symbol{box-t}
  @see-symbol{point3d-t}"
  (apply #'box-init box arg))

(export 'box-init-from-points)

;;; ----------------------------------------------------------------------------
;;; graphene_box_init_from_vec3 ()
;;; ----------------------------------------------------------------------------

(defcfun ("graphene_box_init_from_vec3" box-init-from-vec3)
    (:pointer (:struct box-t))
 #+liber-documentation
 "@version{#2022-9-22}
  @argument[box]{a @symbol{box-t} instance to initialize}
  @argument[min]{a @symbol{vec3-t} instance with the coordinates of the
    minimum vertex}
  @argument[max]{a @symbol{vec3-t} instance with the coordinates of the
    maximum vertex}
  @return{The initialized @symbol{box-t} instance.}
  @begin{short}
    Initializes the given @symbol{box-t} instance with two vertices.
  @end{short}
  @see-symbol{box-t}
  @see-symbol{vec3-t}"
  (box (:pointer (:struct box-t)))
  (min (:pointer (:struct vec3-t)))
  (max (:pointer (:struct vec3-t))))

(export 'box-init-from-vec3)

;;; ----------------------------------------------------------------------------
;;; graphene_box_init_from_vectors ()
;;; ----------------------------------------------------------------------------

(defun box-init-from-vectors (box arg)
 #+liber-documentation
 "@version{#2022-9-22}
  @argument[box]{a @symbol{box-t} instance to initialize}
  @argument[arg]{a list with two @symbol{vec3-t} instances}
  @return{The initialized @symbol{box-t} instance.}
  @begin{short}
    Initializes the given @symbol{box-t} instance with the vertices of
    the given @symbol{vec3-t} instances.
  @end{short}
  @see-symbol{box-t}
  @see-symbol{vec3-t}"
  (apply #'box-init-from-vec3 box arg))

(export 'box-init-from-vectors)

;;; ----------------------------------------------------------------------------
;;; graphene_box_equal ()
;;; ----------------------------------------------------------------------------

(defcfun ("graphene_box_equal" box-equal) :bool
 #+liber-documentation
 "@version{#2022-9-23}
  @argument[a]{a @symbol{box-t} instance to initialize}
  @argument[b]{a @symbol{box-t} instance to initialize}
  @return{@em{True} if the boxes are equal, otherwise @em{false}}
  @begin{short}
    Checks whether the two given boxes are equal.
  @end{short}
  @see-symbol{box-t}"
  (a (:pointer (:struct box-t)))
  (b (:pointer (:struct box-t))))

(export 'box-equal)

;;; ----------------------------------------------------------------------------
;;; graphene_box_expand ()
;;; ----------------------------------------------------------------------------

(defun box-expand (box point result)
 #+liber-documentation
 "@version{#2022-9-23}
  @argument[box]{a @symbol{box-t} instance to expand}
  @argument[point]{a @symbol{point3d-t} instance with the coordinates of the
    point to include}
  @argument[result]{a @symbol{box-t} instance for the result}
  @return{A @symbol{box-t} instance with the result.}
  @begin{short}
    Expands the dimensions of @arg{box} to include the coordinates at
    @arg{point}.
  @end{short}
  @see-symbol{box-t}
  @see-symbol{point3d-t}"
  (foreign-funcall "graphene_box_expand"
                   (:pointer (:struct box-t)) box
                   (:pointer (:struct point3d-t)) point
                   (:pointer (:struct box-t)) result
                   :void)
  result)

(export 'box-expand)

;;; ----------------------------------------------------------------------------
;;; graphene_box_expand_scalar ()
;;; ----------------------------------------------------------------------------

(defun box-expand-scalar (box scalar result)
 #+liber-documentation
 "@version{#2022-9-23}
  @argument[box]{a @symbol{box-t} instance to expand}
  @argument[scalar]{a single float with the scalar value}
  @argument[result]{a @symbol{box-t} instance for the result}
  @return{A @symbol{box-t} instance with the result.}
  @begin{short}
    Expands the dimensions of @arg{box} by the given scalar value.
  @end{short}
  If the @arg{scalar} argument is positive, the box will grow. If the
  @arg{scalar} argument is negative, the box will shrink.
  @see-symbol{box-t}"
  (foreign-funcall "graphene_box_expand_scalar"
                   (:pointer (:struct box-t)) box
                   :float (coerce scalar 'single-float)
                   (:pointer (:struct box-t)) result
                   :void)
  result)

(export 'box-expand-scalar)

;;; ----------------------------------------------------------------------------
;;; graphene_box_expand_vec3 ()
;;; ----------------------------------------------------------------------------

(defun box-expand-vec3 (box vector result)
 #+liber-documentation
 "@version{#2022-9-23}
  @argument[box]{a @symbol{box-t} instance to expand}
  @argument[vector]{a @symbol{vec3-t} instance with the coordinates of the
    point to include}
  @argument[result]{a @symbol{box-t} instance for the result}
  @return{A @symbol{box-t} instance with the result.}
  @begin{short}
    Expands the dimensions of @arg{box} to include the coordinates of the
    given vector.
  @end{short}
  @see-symbol{box-t}
  @see-symbol{vec3-t}"
  (foreign-funcall "graphene_box_expand_vec3"
                   (:pointer (:struct box-t)) box
                   (:pointer (:struct vec3-t)) vector
                   (:pointer (:struct box-t)) result
                   :void)
  result)

(export 'box-expand-vec3)

;;; ----------------------------------------------------------------------------
;;; graphene_box_get_min ()
;;; ----------------------------------------------------------------------------

(defun box-min (box min)
 #+liber-documentation
 "@version{#2022-9-23}
  @argument[box]{a @symbol{box-t} instance}
  @argument[min]{a @symbol{point3d-t} instance with the minimum point}
  @return{A @symbol{point3d-t} instance with the result.}
  @begin{short}
    Retrieves the coordinates of the minimum point of the given box.
  @end{short}
  @see-symbol{box-t}
  @see-symbol{point3d-t}"
  (foreign-funcall "graphene_box_get_min"
                   (:pointer (:struct box-t)) box
                   (:pointer (:struct point3d-t)) min
                   :void)
  min)

(export 'box-min)

;;; ----------------------------------------------------------------------------
;;; graphene_box_get_max ()
;;; ----------------------------------------------------------------------------

(defun box-max (box max)
 #+liber-documentation
 "@version{#2022-9-23}
  @argument[box]{a @symbol{box-t} instance}
  @argument[max]{a @symbol{point3d-t} instance with the maximum point}
  @return{A @symbol{point3d-t} instance with the result.}
  @begin{short}
    Retrieves the coordinates of the maximum point of the given box.
  @end{short}
  @see-symbol{box-t}
  @see-symbol{point3d-t}"
  (foreign-funcall "graphene_box_get_max"
                   (:pointer (:struct box-t)) box
                   (:pointer (:struct point3d-t)) max
                   :void)
  max)

(export 'box-max)

;;; ----------------------------------------------------------------------------
;;; graphene_box_get_center ()
;;; ----------------------------------------------------------------------------

(defun box-center (box center)
 #+liber-documentation
 "@version{#2022-9-23}
  @argument[box]{a @symbol{box-t} instance}
  @argument[center]{a @symbol{point3d-t} instance for the coordinates of the
    center}
  @return{A @symbol{point3d-t} instance with the result.}
  @begin{short}
    Retrieves the coordinates of the center of the box.
  @end{short}
  @see-symbol{box-t}
  @see-symbol{point3d-t}"
  (foreign-funcall "graphene_box_get_center"
                   (:pointer (:struct box-t)) box
                   (:pointer (:struct point3d-t)) center
                   :void)
  center)

(export 'box-center)

;;; ----------------------------------------------------------------------------
;;; graphene_box_get_depth ()
;;; ----------------------------------------------------------------------------

(defcfun ("graphene_box_get_depth" box-depth) :float
 #+liber-documentation
 "@version{#2022-9-23}
  @argument[box]{a @symbol{box-t} instance}
  @return{A single float with the depth of the box.}
  @begin{short}
    Retrieves the size of the box on the z axis.
  @end{short}
  @see-symbol{box-t}"
  (box (:pointer (:struct box-t))))

(export 'box-depth)

;;; ----------------------------------------------------------------------------
;;; graphene_box_get_height ()
;;; ----------------------------------------------------------------------------

(defcfun ("graphene_box_get_height" box-height) :float
 #+liber-documentation
 "@version{#2022-9-23}
  @argument[box]{a @symbol{box-t} instance}
  @return{A single float with the height of the box.}
  @begin{short}
    Retrieves the size of the box on the y axis.
  @end{short}
  @see-symbol{box-t}"
  (box (:pointer (:struct box-t))))

(export 'box-height)

;;; ----------------------------------------------------------------------------
;;; graphene_box_get_width ()
;;; ----------------------------------------------------------------------------

(defcfun ("graphene_box_get_width" box-width) :float
 #+liber-documentation
 "@version{#2022-9-23}
  @argument[box]{a @symbol{box-t} instance}
  @return{A single float with the width of the box.}
  @begin{short}
    Retrieves the size of the box on the x axis.
  @end{short}
  @see-symbol{box-t}"
  (box (:pointer (:struct box-t))))

(export 'box-width)

;;; ----------------------------------------------------------------------------
;;; graphene_box_get_size ()
;;; ----------------------------------------------------------------------------

(defun box-size (box size)
 #+liber-documentation
 "@version{#2022-9-23}
  @argument[box]{a @symbol{box-t} instance}
  @argument[size]{a @symbol{vec3-t} instance for the size}
  @return{A @symbol{vec3-t} instance with the size.}
  @begin{short}
    Retrieves the size of the box on all three axes, and stores it into the
    given size vector.
  @end{short}
  @see-symbol{box-t}
  @see-symbol{vec3-t}"
  (foreign-funcall "graphene_box_get_size"
                   (:pointer (:struct box-t)) box
                   (:pointer (:struct vec3-t)) size
                   :void)
  size)

(export 'box-size)

;;; ----------------------------------------------------------------------------
;;; graphene_box_get_bounding_sphere ()
;;; ----------------------------------------------------------------------------

(defun box-bounding-sphere (box sphere)
 #+liber-documentation
 "@version{#2022-9-23}
  @argument[box]{a @symbol{box-t} instance}
  @argument[sphere]{a @symbol{sphere-t} instance for the bounding sphere}
  @return{A @symbol{sphere-t} instance with the bounding sphere.}
  @begin{short}
    Computes the bounding sphere capable of containing the given box.
  @end{short}
  @see-symbol{box-t}
  @see-symbol{sphere-t}"
  (foreign-funcall "graphene_box_get_bounding_sphere"
                   (:pointer (:struct box-t)) box
                   (:pointer (:struct sphere-t)) sphere
                   :void)
  sphere)

(export 'box-bounding-sphere)

;;; ----------------------------------------------------------------------------
;;; graphene_box_get_vertices ()
;;; ----------------------------------------------------------------------------

(defun box-vertices (box vertices)
 #+liber-documentation
 "@version{#2022-9-23}
  @argument[box]{a @symbol{box-t} instance}
  @argument[vertices]{a list of eigth @symbol{vec3-t} instances}
  @return{A list of  eight @symbol{vec3-t} instances with the vertices.}
  @begin{short}
    Computes the vertices of the given box.
  @end{short}
  @begin[Example]{dictionary}
    @begin{pre}
(with-graphene-vec3s (v0 v1 v2 v3 v4 v5 v6 v7)
  (with-graphene-point3ds ((min 0 0 0) (max 1 1 1))
    (with-graphene-box (box min max)
      (mapcar #'vec3-to-float
              (box-vertices box (list v0 v1 v2 v3 v4 v5 v6 v7))))))
=> ((0.0 0.0 0.0) (0.0 0.0 1.0) (0.0 1.0 0.0) (0.0 1.0 1.0)
    (1.0 0.0 0.0) (1.0 0.0 1.0) (1.0 1.0 0.0) (1.0 1.0 1.0))
    @end{pre}
  @end{dictionary}
  @see-symbol{box-t}
  @see-symbol{vec3-t}"
  (with-graphene-point3ds (min max)
    (box-min box min)
    (box-max box max)
    (let ((v0 (elt vertices 0))
          (v1 (elt vertices 1))
          (v2 (elt vertices 2))
          (v3 (elt vertices 3))
          (v4 (elt vertices 4))
          (v5 (elt vertices 5))
          (v6 (elt vertices 6))
          (v7 (elt vertices 7)))
      (vec3-init v0 (point3d-x min) (point3d-y min) (point3d-z min))
      (vec3-init v1 (point3d-x min) (point3d-y min) (point3d-z max))
      (vec3-init v2 (point3d-x min) (point3d-y max) (point3d-z min))
      (vec3-init v3 (point3d-x min) (point3d-y max) (point3d-z max))
      (vec3-init v4 (point3d-x max) (point3d-y min) (point3d-z min))
      (vec3-init v5 (point3d-x max) (point3d-y min) (point3d-z max))
      (vec3-init v6 (point3d-x max) (point3d-y max) (point3d-z min))
      (vec3-init v7 (point3d-x max) (point3d-y max) (point3d-z max))
      (list v0 v1 v2 v3 v4 v5 v6 v7))))

(export 'box-vertices)

;;; ----------------------------------------------------------------------------
;;; graphene_box_union ()
;;; ----------------------------------------------------------------------------

(defun box-union (a b result)
 #+liber-documentation
 "@version{#2022-9-23}
  @argument[a]{a @symbol{box-t} instance}
  @argument[b]{a @symbol{box-t} instance}
  @argument[result]{a @symbol{box-t} instance for the result}
  @return{A @symbol{box-t} instance with the result.}
  @begin{short}
    Unions the two given boxes.
  @end{short}
  @see-symbol{box-t}"
  (foreign-funcall "graphene_box_union"
                   (:pointer (:struct box-t)) a
                   (:pointer (:struct box-t)) b
                   (:pointer (:struct box-t)) result
                   :void)
  result)

(export 'box-union)

;;; ----------------------------------------------------------------------------
;;; graphene_box_intersection ()
;;; ----------------------------------------------------------------------------

(defun box-intersection (a b result)
 #+liber-documentation
 "@version{#2022-9-23}
  @syntax[]{(box-intersection a b result) => result, success}
  @argument[a]{a @symbol{box-t} instance}
  @argument[b]{a @symbol{box-t} instance}
  @argument[result]{a @symbol{box-t} instance for the result}
  @argument[success]{@em{true} if the two boxes intersect}
  @begin{short}
    Intersects the two given boxes.
  @end{short}
  If the two boxes do not intersect, @arg{result} will contain a degenerate box
  initialized with the @fun{box-empty} values.
  @see-symbol{box-t}
  @see-function{box-empty}"
  (let ((success (foreign-funcall "graphene_box_union"
                                  (:pointer (:struct box-t)) a
                                  (:pointer (:struct box-t)) b
                                  (:pointer (:struct box-t)) result
                                  :bool)))
    (values result success)))

(export 'box-intersection)

;;; ----------------------------------------------------------------------------
;;; graphene_box_contains_box ()
;;; ----------------------------------------------------------------------------

(defcfun ("graphene_box_contains_box" box-contains-box) :bool
 #+liber-documentation
 "@version{#2022-9-23}
  @argument[a]{a @symbol{box-t} instance}
  @argument[b]{a @symbol{box-t} instance}
  @return{@em{True} if @arg{b} is contained in @arg{a}.}
  @begin{short}
    Checks whether the the box @arg{a} contains the given box @arg{b}.
  @end{short}
  @see-symbol{box-t}"
  (a (:pointer (:struct box-t)))
  (b (:pointer (:struct box-t))))

(export 'box-contains-box)

;;; ----------------------------------------------------------------------------
;;; graphene_box_contains_point ()
;;; ----------------------------------------------------------------------------

(defcfun ("graphene_box_contains_point" box-contains-point) :bool
 #+liber-documentation
 "@version{#2022-9-23}
  @argument[box]{a @symbol{box-t} instance}
  @argument[point]{a @symbol{point3d-t} instance with the coordiates to check}
  @return{@em{True} if the point is contained in the box.}
  @begin{short}
    Checks whether the box contains the given point.
  @end{short}
  @see-symbol{box-t}
  @see-symbol{point3d-t}"
  (a (:pointer (:struct box-t)))
  (b (:pointer (:struct point3d-t))))

(export 'box-contains-point)

;;; ----------------------------------------------------------------------------
;;; graphene_box_zero ()
;;; ----------------------------------------------------------------------------

(defcfun ("graphene_box_zero" box-zero) (:pointer (:struct box-t))
 #+liber-documentation
 "@version{#2022-9-23}
  @return{A @symbol{box-t} instance.}
  @begin{short}
    Returns a box with both the minimum and maximum vertices set at (0, 0, 0).
  @end{short}
  The returned box is owned by Graphene and should not be modified or freed.
  @see-symbol{box-t}")

(export 'box-zero)

;;; ----------------------------------------------------------------------------
;;; graphene_box_one ()
;;; ----------------------------------------------------------------------------

(defcfun ("graphene_box_one" box-one) (:pointer (:struct box-t))
 #+liber-documentation
 "@version{#2022-9-23}
  @return{A @symbol{box-t} instance.}
  @begin{short}
    Returns a box with both the minimum and maximum vertices set at (1, 1, 1).
  @end{short}
  The returned box is owned by Graphene and should not be modified or freed.
  @see-symbol{box-t}")

(export 'box-one)

;;; ----------------------------------------------------------------------------
;;; graphene_box_minus_one ()
;;; ----------------------------------------------------------------------------

(defcfun ("graphene_box_minus_one" box-minus-one) (:pointer (:struct box-t))
 #+liber-documentation
 "@version{#2022-9-23}
  @return{A @symbol{box-t} instance.}
  @begin{short}
    Returns a box with the minimum vertex set at (-1, -1, -1) and the maximum
    vertex set at (0, 0, 0).
  @end{short}
  The returned box is owned by Graphene and should not be modified or freed.
  @see-symbol{box-t}")

(export 'box-minus-one)

;;; ----------------------------------------------------------------------------
;;; graphene_box_one_minus_one ()
;;; ----------------------------------------------------------------------------

(defcfun ("graphene_box_one_minus_one" box-one-minus-one)
    (:pointer (:struct box-t))
 #+liber-documentation
 "@version{#2022-9-23}
  @return{A @symbol{box-t} instance.}
  @begin{short}
    Returns a box the minimum vertex set at (-1, -1, -1) and the maximum
    vertex set at (1, 1, 1).
  @end{short}
  The returned box is owned by Graphene and should not be modified or freed.
  @see-symbol{box-t}")

(export 'box-one-minus-one)

;;; ----------------------------------------------------------------------------
;;; graphene_box_empty ()
;;; ----------------------------------------------------------------------------

(defcfun ("graphene_box_empty" box-empty) (:pointer (:struct box-t))
 #+liber-documentation
 "@version{#2022-9-23}
  @return{A @symbol{box-t} instance.}
  @begin{short}
    Returns a degenerate box that can only be expanded.
  @end{short}
  The returned box is owned by Graphene and should not be modified or freed.
  @see-symbol{box-t}")

(export 'box-empty)

;;; ----------------------------------------------------------------------------
;;; graphene_box_infinite ()
;;; ----------------------------------------------------------------------------

(defcfun ("graphene_box_infinite" box-infinite) (:pointer (:struct box-t))
 #+liber-documentation
 "@version{#2022-9-23}
  @return{A @symbol{box-t} instance.}
  @begin{short}
    Returns a degenerate box that cannot be expanded.
  @end{short}
  The returned box is owned by Graphene and should not be modified or freed.
  @see-symbol{box-t}")

(export 'box-infinite)

;;; --- End of file graphene.box.lisp ------------------------------------------
