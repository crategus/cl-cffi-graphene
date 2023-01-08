;;; ----------------------------------------------------------------------------
;;; graphene.point3d.lisp
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
;;; Point3D
;;;
;;;     A point with 3 coordinates
;;;
;;; Types and Values
;;;
;;;     graphene_point3d_t
;;;
;;; Functions
;;;
;;;     graphene_point3d_alloc
;;;     graphene_point3d_free
;;;     graphene_point3d_zero
;;;     graphene_point3d_init
;;;     graphene_point3d_init_from_point
;;;     graphene_point3d_init_from_vec3
;;;     graphene_point3d_to_vec3
;;;     graphene_point3d_equal
;;;     graphene_point3d_near
;;;     graphene_point3d_distance
;;;     graphene_point3d_interpolate
;;;     graphene_point3d_scale
;;;     graphene_point3d_cross
;;;     graphene_point3d_dot
;;;     graphene_point3d_length
;;;     graphene_point3d_normalize
;;;     graphene_point3d_normalize_viewport
;;; ----------------------------------------------------------------------------

(in-package :graphene)

(defmacro with-graphene-point3d ((var &rest args) &body body)
 #+liber-documentation
 "@version{#2022-10-1}
  @syntax[]{with-graphene-point3d (p) body => result}
  @syntax[]{with-graphene-point3d (p x y z) body => result}
  @syntax[]{with-graphene-point3d (p p1) body => result}
  @syntax[]{with-graphene-point3d (p (v vec3-t)) body => result}
  @argument[p]{a @symbol{point3d-t} instance to create and initialize}
  @argument[a]{a number coerced to a single float for the x component of the
    point}
  @argument[b]{a number coerced to a single float for the y component of the
    point}
  @argument[p1]{a @symbol{point3d-t} instance to use for initialization}
  @argument[v]{a @symbol{vec3-t} instance to use for initialization}
  @begin{short}
    The @sym{with-gaphene-point3d} macro allocates a new @symbol{point3d-t}
    instance, initializes the point with the given values and executes the body
    that uses the point.
  @end{short}
  After execution of the body the allocated memory for the point is released.

  When no argument is given the components of the point are initialized to zero.
  The initialization with three single float values uses the @fun{point3d-init}
  function. The initialization from another point is done with the
  @fun{point3d-init-from-point} function. That is the default when no type
  specifier for the value is given. If the value has the type specifier
  @code{vec3-t} the point is initialized with the @fun{point3d-init-from-vec3}
  function.
  @begin[Note]{dictionary}
    The memory is allocated with the @fun{point3d-alloc} function and released
    with the @fun{point3d-free} function.
  @end{dictionary}
  @begin[Examples]{dictionary}
    Initialize a point with no value and two single float values.
    @begin{pre}
(with-graphene-point3d (p)
  (list (point3d-x p) (point3d-y p) (point3d-z p)))
=> (0.0 0.0 0.0)
(with-graphene-point3d (p 1.5 1.7 1.9)
  (list (point3d-x p) (point3d-y p) (point3d-z p)))
=> (1.5 1.7 1.9)
    @end{pre}
    Use a vector for initialization of the point.
    @begin{pre}
(with-graphene-vec3 (v 3.5 4.5 5.5)
  (with-graphene-point3d (p (v vec3-t))
    (list (point3d-x p) (point3d-y p) (point3d-z p))))
=> (3.5 4.5 5.5)
    @end{pre}
    This examples uses the @fun{with-graphene-point3ds} macro to initialize
    two points. The second point is intialized with the values from the
    first point.
    @begin{pre}
(with-graphene-point3ds ((p1 0.3 0.5 0.7) (p2 p1))
  (list (point3d-x p2) (point3d-y p2) (point3d-z p2)))
=> (0.3 0.5 0.7)
    @end{pre}
  @end{dictionary}
  @see-symbol{point3d-t}
  @see-symbol{vec3-t}
  @see-macro{with-graphene-point3ds}
  @see-function{point3d-alloc}
  @see-function{point3d-free}"
  (cond ((not args)
         ;; We have no arguments, the default is initialization with zeros.
         `(let ((,var (point3d-alloc)))
            (point3d-init ,var 0.0 0.0 0.0)
            (unwind-protect
              (progn ,@body)
              (point3d-free ,var))))
        ((not (second args))
         ;; We have one argument. The argument is of type point3d-t or vec3-t
         (destructuring-bind (arg &optional type)
             (if (listp (first args)) (first args) (list (first args)))
           (cond ((or (not type)
                      (eq type 'point3d-t))
                  ;; One argument with no type or of type point3d-t
                  `(let ((,var (point3d-alloc)))
                     (point3d-init-from-point ,var ,arg)
                     (unwind-protect
                       (progn ,@body)
                       (point3d-free ,var))))
                 ((eq type 'vec3-t)
                  ;; One argument with type vec3-t
                  `(let ((,var (point3d-alloc)))
                     (point3d-init-from-vec3 ,var ,arg)
                     (unwind-protect
                       (progn ,@body)
                       (point3d-free ,var))))
                 (t
                  (error "Type error in WITH-GRAPHENE-POINT3D")))))
        ((not (fourth args))
         ;; We have a list of three arguments with (x,y,z) values
         `(let ((,var (point3d-alloc)))
            (point3d-init ,var ,@args)
            (unwind-protect
              (progn ,@body)
              (point3d-free ,var))))
        (t
         (error "Syntax error in WITH-GRAPHENE-POINT3D"))))

(export 'with-graphene-point3d)

(defmacro with-graphene-point3ds (vars &body body)
 #+liber-documentation
 "@version{#2022-10-1}
  @syntax[]{with-graphene-point3ds (p1 p2 p3 ... pn) body => result}
  @argument[p1 ... pn]{the newly created @symbol{point3d-t} instances}
  @argument[body]{a body that uses the bindings @arg{p1} ... @arg{pn}}
  @begin{short}
    The @sym{with-graphene-point3ds} macro creates new variable bindings and
    executes the body that use these bindings.
  @end{short}
  The macro performs the bindings sequentially, like the @sym{let*} macro.

  Each point can be initialized with values using the syntax for the
  @fun{with-graphene-point3d} macro. See also the @fun{within-graphene-point3d}
  documentation.
  @begin[Examples]{dictionary}
    @begin{pre}
(with-graphene-point3ds (p1 (p2 1.2 1.3 1.4) (p3 p2))
  (list (list (point3d-x p1) (point3d-y p1) (point3d-z p1))
        (list (point3d-x p2) (point3d-y p2) (point3d-z p2))
        (list (point3d-x p3) (point3d-y p3) (point3d-z p3))))
=> ((0.0 0.0 0.0) (1.2 1.3 1.4) (1.2 1.3 1.4))
    @end{pre}
  @end{dictionary}
  @see-symbol{point3d-t}
  @see-macro{with-graphene-point3d}"
  (if vars
      (let ((var (if (listp (first vars)) (first vars) (list (first vars)))))
        `(with-graphene-point3d ,var
           (with-graphene-point3ds ,(rest vars)
             ,@body)))
      `(progn ,@body)))

(export 'with-graphene-point3ds)

;;; ----------------------------------------------------------------------------
;;; graphene_point3d_t
;;; ----------------------------------------------------------------------------

(defcstruct point3d-t
  (x :float)
  (y :float)
  (z :float))

#+liber-documentation
(setf (liber:alias-for-symbol 'point3d-t)
      "CStruct"
      (liber:symbol-documentation 'point3d-t)
 "@version{#2022-10-1}
  @begin{short}
  The @sym{point3d-t} structure is a data structure capable of describing a
  point with three coordinates.
  @end{short}
  @begin{pre}
(defcstruct point3d-t
  (x :float)
  (y :float)
  (z :float))
  @end{pre}
  Access the coordinates with the @fun{point3d-x}, @fun{point3d-y} and
  @fun{point3d-z} functions.
  @see-symbol{point-t}
  @see-function{point3d-x}
  @see-function{point3d-y}
  @see-function{point3d-z}")

(export 'point3d-t)

;;; --- Acessor Implementations ------------------------------------------------

;;; --- point3d-x --------------------------------------------------------------

(defun (setf point3d-x) (value point)
  (setf (foreign-slot-value point '(:struct point3d-t) 'x) value))

(defun point3d-x (point)
  (foreign-slot-value point '(:struct point3d-t) 'x))

#+liber-documentation
(setf (liber:alias-for-function 'point3d-x)
      "Accessor"
      (documentation 'point3d-x 'function)
 "@version{#2022-10-1}
  @syntax[]{(point3d-x point) => x}
  @syntax[]{(setf (point3d-x point) x)}
  @argument[point]{a @symbol{point3d-t} instance}
  @argument[x]{a single float with the x coordinate}
  @begin{short}
    Accessor of the @code{x} slot of the @symbol{point3d-t} structure.
  @end{short}
  @begin[Examples]{dictionary}
    @begin{pre}
(with-graphene-point3d (p 0.5 1.0 1.5)
  (list (point3d-x p) (point3d-y p) (point3d-z p)))
=> (0.5 1.0 1.5)
(with-graphene-point3d (p)
  (setf (point3d-x p) 2.0 (point3d-y p) 2.5 (point3d-z p) 3.0)
  (list (point3d-x p) (point3d-y p) (point3d-z p)))
=> (2.0 2.5 3.0)
  @end{pre}
  @end{dictionary}
  @see-symbol{point3d-t}")

(export 'point3d-x)

;;; --- point3d-y --------------------------------------------------------------

(defun (setf point3d-y) (value point)
  (setf (foreign-slot-value point '(:struct point3d-t) 'y) value))

(defun point3d-y (point)
  (foreign-slot-value point '(:struct point3d-t) 'y))

#+liber-documentation
(setf (liber:alias-for-function 'point3d-y)
      "Accessor"
      (documentation 'point3d-y 'function)
 "@version{#2022-10-1}
  @syntax[]{(point3d-y point) => y}
  @syntax[]{(setf (point3d-y point) y)}
  @argument[point]{a @symbol{point3d-t} instance}
  @argument[x]{a single float with the y coordinate}
  @begin{short}
    Accessor of the @code{y} slot of the @symbol{point3d-t} structure.
  @end{short}
  See the @fun{point3d-x} documentation for examples.
  @see-symbol{point3d-t}
  @see-function{point3d-x}")

(export 'point3d-y)

;;; --- point3d-z --------------------------------------------------------------

(defun point3d-z (point)
  (foreign-slot-value point '(:struct point3d-t) 'z))

(defun (setf point3d-z) (value point)
  (setf (foreign-slot-value point '(:struct point3d-t) 'z) value))

#+liber-documentation
(setf (liber:alias-for-function 'point3d-z)
      "Accessor"
      (documentation 'point3d-z 'function)
 "@version{#2022-10-1}
  @syntax[]{(point3d-z point) => z}
  @syntax[]{(setf (point3d-z point) z)}
  @argument[point]{a @symbol{point3d-t} instance}
  @argument[x]{a single float with the z coordinate}
  @begin{short}
    Accessor of the @code{z} slot of the @symbol{point3d-t} structure.
  @end{short}
  See the @fun{point3d-x} documentation for examples.
  @see-symbol{point3d-t}
  @see-function{point3d-x}")

(export 'point3d-z)

;;; ----------------------------------------------------------------------------
;;; graphene_point3d_alloc ()
;;; ----------------------------------------------------------------------------

(defcfun ("graphene_point3d_alloc" point3d-alloc)
    (:pointer (:struct point3d-t))
 #+liber-documentation
 "@version{#2022-10-1}
  @return{The newly allocated @symbol{point3d-t} instance.}
  @begin{short}
    Allocates a new @symbol{point3d-t} instance.
  @end{short}
  The coordinates of the returned point are initialized to (0.0, 0.0, 0.0). Use
  the @fun{point3d-free} function to free the resources allocated by this
  function.
  @begin[Examples]{dictionary}
    It is possible to chain this function with the @fun{point3d-init} or
    @fun{point3d-init-from-point} functions.
    @begin{pre}
(defun point3d-new (x y z)
  (point3d-init (point3d-alloc) x y z))
(defun point3d-copy (p)
  (point3d-init-from-point (point3d-alloc) p))
    @end{pre}
  @end{dictionary}
  @see-symbol{point3d-t}
  @see-function{point-free}
  @see-function{point3d-init}
  @see-function{point3d-init-from-point}")

(export 'point3d-alloc)

;;; ----------------------------------------------------------------------------
;;; graphene_point3d_free ()
;;; ----------------------------------------------------------------------------

(defcfun ("graphene_point3d_free" point3d-free) :void
 #+liber-documentation
 "@version{#2022-10-1}
  @argument[point]{a @symbol{point3d-t} instance}
  @begin{short}
    Frees the resources allocated by the @fun{point3d-alloc} function.
  @end{short}
  @see-symbol{point3d-t}
  @see-function{point3d-alloc}"
  (point (:pointer (:struct point3d-t))))

(export 'point3d-free)

;;; ----------------------------------------------------------------------------
;;; graphene_point3d_zero ()
;;; ----------------------------------------------------------------------------

(defcfun ("graphene_point3d_zero" point3d-zero)
    (:pointer (:struct point3d-t))
 #+liber-documentation
 "@version{#2022-10-1}
  @return{The @symbol{point3d-t} instance with a zero point.}
  @begin{short}
    Returns a point with all three coordinates set to zero.
  @end{short}
  @begin[Examples]{dictionary}
    @begin{pre}
(values (point3d-x (point3d-zero))
        (point3d-y (point3d-zero))
        (point3d-z (point3d-zero)))
=> 0.0
=> 0.0
=> 0.0
    @end{pre}
  @end{dictionary}
  @see-symbol{point3d-t}")

(export 'point3d-zero)

;;; ----------------------------------------------------------------------------
;;; graphene_point3d_init ()
;;; ----------------------------------------------------------------------------

(defun point3d-init (point x y z)
 #+liber-documentation
 "@version{#2022-10-1}
  @argument[point]{a @symbol{point3d-t} instance}
  @argument[x]{a number coerced to a single float with the x coordinate}
  @argument[y]{a number coerced to a single float with the y coordinate}
  @argument[z]{a number coerced to a single float with the z coordinate}
  @return{The initialized @symbol{point3d-t} instance.}
  @begin{short}
    Initializes the point to the given @arg{x}, @arg{y}, and @arg{z}
    coordinates.
  @end{short}
  It is safe to call this function multiple times.
  @see-symbol{point3d-t}"
  (foreign-funcall "graphene_point3d_init"
                   (:pointer (:struct point3d-t)) point
                   :float (coerce x 'single-float)
                   :float (coerce y 'single-float)
                   :float (coerce z 'single-float)
                   (:pointer (:struct point3d-t))))

(export 'point3d-init)

;;; ----------------------------------------------------------------------------
;;; graphene_point3d_init_from_point ()
;;; ----------------------------------------------------------------------------

(defcfun ("graphene_point3d_init_from_point" point3d-init-from-point)
    (:pointer (:struct point3d-t))
 #+liber-documentation
 "@version{#2022-10-1}
  @argument[point]{a @symbol{point3d-t} instance}
  @argument[source]{a @symbol{point3d-t} instance to use}
  @return{The initialized @symbol{point3d-t} instance.}
  @short{Initializes the point using the coordinates of @arg{source}.}
  @see-symbol{point3d-t}"
  (point (:pointer (:struct point3d-t)))
  (source (:pointer (:struct point3d-t))))

(export 'point3d-init-from-point)

;;; ----------------------------------------------------------------------------
;;; graphene_point3d_init_from_vec3 ()
;;; ----------------------------------------------------------------------------

(defcfun ("graphene_point3d_init_from_vec3" point3d-init-from-vec3)
   (:pointer (:struct point3d-t))
 #+liber-documentation
 "@version{#2022-10-1}
  @argument[point]{a @symbol{point3d-t} instance}
  @argument[vector]{a @symbol{vec3-t} instance to use}
  @return{The initialized @symbol{point3d-t} instance.}
  @short{Initializes the point using the components of the given vector.}
  @see-symbol{point3d-t}
  @see-symbol{vec3-t}"
  (point (:pointer (:struct point3d-t)))
  (vector :pointer)) ; vec3-t not known at this point

(export 'point3d-init-from-vec3)

;;; ----------------------------------------------------------------------------
;;; graphene_point3d_to_vec3 ()
;;; ----------------------------------------------------------------------------

(defun point3d-to-vec3 (point vector)
 #+liber-documentation
 "@version{#2022-10-1}
  @argument[point]{a @symbol{point3d-t} instance}
  @argument[vector]{a @symbol{vec3-t} instance}
  @return{The @symbol{vec3-t} instance with the coordinates of the point.}
  @short{Stores the coordinates of the given point into a vector.}
  @begin[Examples]{dictionary}
    @begin{pre}
(with-graphene-point3d (p 1.0 2.0 3.0)
  (with-graphene-vec3 (v)
    (vec3-to-float (point3d-to-vec3 p v))))
=> (1.0 2.0 3.0)
    @end{pre}
  @end{dictionary}
  @see-symbol{point3d-t}
  @see-symbol{vec3-t}"
  (foreign-funcall "graphene_point3d_to_vec3"
                   (:pointer (:struct point3d-t)) point
                   :pointer vector ; vec3-t not known at this point
                   :void)
  vector)

(export 'point3d-to-vec3)

;;; ----------------------------------------------------------------------------
;;; graphene_point3d_equal ()
;;; ----------------------------------------------------------------------------

(defcfun ("graphene_point3d_equal" point3d-equal) :bool
 #+liber-documentation
 "@version{#2022-10-1}
  @argument[a]{a @symbol{point3d-t} instance}
  @argument[b]{a @symbol{point3d-t} instance}
  @return{@em{True}, if the points have the same coordinates, otherwise
    @em{false}.}
  @begin{short}
    Checks whether two given points are equal.
  @end{short}
  This function accounts for floating point fluctuations. If you want to
  control the fuzziness of the match, you can use the @fun{point3d-near}
  function instead.
  @see-symbol{point3d-t}
  @see-function{point3d-near}"
  (a (:pointer (:struct point3d-t)))
  (b (:pointer (:struct point3d-t))))

(export 'point3d-equal)

;;; ----------------------------------------------------------------------------
;;; graphene_point3d_near ()
;;; ----------------------------------------------------------------------------

(defun point3d-near (a b epsilon)
 #+liber-documentation
 "@version{#2022-10-1}
  @argument[a]{a @symbol{point3d-t} instance}
  @argument[b]{a @symbol{point3d-t} instance}
  @argument[epsilon]{a number coerced to a single float with the threshold
    between the two points}
  @return{@em{True}, if the distance between the points is within
    @arg{epsilon}, otherwise @em{false}.}
  @begin{short}
    Checks whether the two points @arg{a} and @arg{b} are within the threshold
    of @arg{epsilon} equal.
  @end{short}
  @see-symbol{point-t}
  @see-function{point3d-equal}"
  (foreign-funcall "graphene_point3d_near"
                   (:pointer (:struct point3d-t)) a
                   (:pointer (:struct point3d-t)) b
                   :float (coerce epsilon 'single-float)
                   :bool))

(export 'point3d-near)

;;; ----------------------------------------------------------------------------
;;; graphene_point3d_distance ()
;;; ----------------------------------------------------------------------------

(defun point3d-distance (a b delta)
 #+liber-documentation
 "@version{#2022-10-1}
  @syntax[]{(point-distance a b) => distance, delta}
  @argument[a]{a @symbol{point3d-t} instance}
  @argument[b]{a @symbol{point3d-t} instance}
  @argument[distance]{a single float with the distance between the two points}
  @argument[delta]{a @symbol{vec3-t} instance with the distance compoments
    on the x, y, and z axis}
  @short{Computes the distance between the two given points.}
  @see-symbol{point3d-t}
  @see-symbol{vec3-t}"
  (values (foreign-funcall "graphene_point3d_distance"
                           (:pointer (:struct point3d-t)) a
                           (:pointer (:struct point3d-t)) b
                           :pointer delta ; vec3-t not known
                           :float)
          delta))

(export 'point3d-distance)

;;; ----------------------------------------------------------------------------
;;; graphene_point3d_interpolate ()
;;; ----------------------------------------------------------------------------

(defun point3d-interpolate (a b factor result)
 #+liber-documentation
 "@version{#2022-10-1}
  @argument[a]{a @symbol{point3d-t} instance}
  @argument[b]{a @symbol{point3d-t} instance}
  @argument[factor]{a number coerced to a double float with the linear
    interpolation factor}
  @argument[result]{a @symbol{point3d-t} instance for the interpolated point}
  @return{The @symbol{point3d-t} instance with the interpolated point.}
  @begin{short}
    Linearly interpolates the coordinates of @arg{a} and @arg{b} using the
    given @arg{factor}.
  @end{short}
  @begin[Examples]{dictionary}
    @begin{pre}
(with-graphene-point3ds ((a 0 0 0) (b 1 2 3) result)
  (point3d-interpolate a b 0.5 result)
  (values (point3d-x result) (point3d-y result) (point3d-z result)))
=> 0.5
=> 1.0
=> 1.5
    @end{pre}
  @end{dictionary}
  @see-symbol{point3d-t}"
  (foreign-funcall "graphene_point3d_interpolate"
                   (:pointer (:struct point3d-t)) a
                   (:pointer (:struct point3d-t)) b
                   :double (coerce factor 'double-float)
                   (:pointer (:struct point3d-t)) result
                   :void)
  result)

(export 'point3d-interpolate)

;;; ----------------------------------------------------------------------------
;;; graphene_point3d_scale ()
;;; ----------------------------------------------------------------------------

(defun point3d-scale (point factor result)
 #+liber-documentation
 "@version{#2022-10-1}
  @argument[point]{a @symbol{point3d-t} instance}
  @argument[factor]{a number coerced to a single float with the scaling factor}
  @argument[result]{a @symbol{point3d-t} instance for the scaled point}
  @return{The @symbol{point3d-t} instance with the scaled point.}
  @begin{short}
    Scales the coordinates of the given point by the given factor.
  @end{short}
  @see-symbol{point3d-t}"
  (foreign-funcall "graphene_point3d_scale"
                   (:pointer (:struct point3d-t)) point
                   :float (coerce factor 'single-float)
                   (:pointer (:struct point3d-t)) result
                   :void)
  result)

(export 'point3d-scale)

;;; ----------------------------------------------------------------------------
;;; graphene_point3d_cross ()
;;; ----------------------------------------------------------------------------

(defun point3d-cross (a b result)
 #+liber-documentation
 "@version{#2022-10-1}
  @argument[a]{a @symbol{point3d-t} instance}
  @argument[b]{a @symbol{point3d-t} instance}
  @argument[result]{a @symbol{point3d-t} instance for the cross product}
  @return{The @symbol{point3d-t} instance with the cross product.}
  @short{Computes the cross product of the two given points.}
  @see-symbol{point3d-t}"
  (foreign-funcall "graphene_point3d_cross"
                   (:pointer (:struct point3d-t)) a
                   (:pointer (:struct point3d-t)) b
                   (:pointer (:struct point3d-t)) result
                   :void)
  result)

(export 'point3d-cross)

;;; ----------------------------------------------------------------------------
;;; graphene_point3d_dot ()
;;; ----------------------------------------------------------------------------

(defcfun ("graphene_point3d_dot" point3d-dot) :float
 #+liber-documentation
 "@version{#2022-10-1}
  @argument[a]{a @symbol{point3d-t} instance}
  @argument[b]{a @symbol{point3d-t} instance}
  @return{A single float with the value of the dot product.}
  @short{Computes the dot product of the two given points.}
  @see-symbol{point3d-t}"
  (a (:pointer (:struct point3d-t)))
  (b (:pointer (:struct point3d-t))))

(export 'point3d-dot)

;;; ----------------------------------------------------------------------------
;;; graphene_point3d_length ()
;;; ----------------------------------------------------------------------------

(defcfun ("graphene_point3d_length" point3d-length) :float
 #+liber-documentation
 "@version{#2022-10-1}
  @argument[point]{a @symbol{point3d-t} instance}
  @return{A single float with the value of the length of the vector represented
    by the point.}
  @begin{short}
    Computes the length of the vector represented by the coordinates of the
    given point.
  @end{short}
  @see-symbol{point3d-t}"
  (point (:pointer (:struct point3d-t))))

(export 'point3d-length)

;;; ----------------------------------------------------------------------------
;;; graphene_point3d_normalize ()
;;; ----------------------------------------------------------------------------

(defun point3d-normalize (point result)
 #+liber-documentation
 "@version{#2022-10-1}
  @argument[point]{a @symbol{point3d-t} instance}
  @return{A @symbol{point3d-t} instance with the normalized point.}
  @begin{short}
    Computes the normalization of the vector represented by the coordinates of
    the given point.
  @end{short}
  @see-symbol{point3d-t}"
  (foreign-funcall "graphene_point3d_normalize"
                   (:pointer (:struct point3d-t)) point
                   (:pointer (:struct point3d-t)) result
                   :void)
  result)

(export 'point3d-normalize)

;;; ----------------------------------------------------------------------------
;;; graphene_point3d_normalize_viewport ()
;;; ----------------------------------------------------------------------------

(defun point3d-normalize-viewport (point viewport znear zfar result)
 #+liber-documentation
 "@version{#2022-10-1}
  @argument[point]{a @symbol{point3d-t} instance}
  @argument[viewport]{a @symbol{rect-t} instance representing a viewport}
  @argument[znear]{a number coerced to a single float with the coordinate of
    the near clipping plane, of 0.0 for the default near clipping plane}
  @argument[zfar]{a number coerced to a single float with the coordinate of
    the far clipping plane, of 1.0 for the default far clipping plane}
  @argument[point]{a @symbol{point3d-t} instance}
  @argument[result]{a @symbol{point3d-t} instance for the nomalized point}
  @return{A @symbol{point3d-t} instance with the normalized point.}
  @begin{short}
    Normalizes the coordinates of the point using the given viewport and
    clipping planes.
  @end{short}
  The coordinates of the resulting point will be in the [-1.0, 1.0] range.
  @see-symbol{point3d-t}
  @see-symbol{rect-t}"
  (foreign-funcall "graphene_point3d_normalize_viewport"
                   (:pointer (:struct point3d-t)) point
                   (:pointer (:struct rect-t)) viewport
                   :float (coerce znear 'single-float)
                   :float (coerce zfar 'single-float)
                   (:pointer (:struct point3d-t)) result
                   :void)
  result)

(export 'point3d-normalize-viewport)

;;; --- End of file graphene-point3d.lisp --------------------------------------
