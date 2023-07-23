;;; ----------------------------------------------------------------------------
;;; graphene.point.lisp
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
;;; Point
;;;
;;;     A point with 2 coordinates
;;;
;;; Types and Values
;;;
;;;     graphene_point_t
;;;
;;; Functions
;;;
;;;     graphene_point_alloc
;;;     graphene_point_free
;;;     graphene_point_zero
;;;     graphene_point_init
;;;     graphene_point_init_from_point
;;;     graphene_point_init_from_vec2
;;;     graphene_point_to_vec2
;;;     graphene_point_equal
;;;     graphene_point_near
;;;     graphene_point_distance
;;;     graphene_point_interpolate
;;; ----------------------------------------------------------------------------

(in-package :graphene)

(defmacro with-graphene-point ((var &rest args) &body body)
 #+liber-documentation
 "@version{#2022-9-29}
  @syntax[]{with-graphene-point (p) body => result}
  @syntax[]{with-graphene-point (p x y) body => result}
  @syntax[]{with-graphene-point (p p1) body => result}
  @syntax[]{with-graphene-point (p (v vec2-t)) body => result}
  @argument[p]{a @symbol{point-t} instance to create and initialize}
  @argument[x]{a number coerced to a single float for the x component of the
    point}
  @argument[y]{a number coerced to a single float for the y component of the
    point}
  @argument[p1]{a @symbol{point-t} instance to use for initialization}
  @argument[v]{a @symbol{vec2-t} instance to use for initialization}
  @begin{short}
    The @sym{with-gaphene-point} macro allocates a new @symbol{point-t}
    instance, initializes the point with the given values and executes the body
    that uses the point.
  @end{short}
  After execution of the body the allocated memory for the point is released.

  When no argument is given the components of the point are initialized to zero.
  The initialization with two single float values uses the @fun{point-init}
  function. The initialization from another point is done with the
  @fun{point-init-from-point} function. That is the default when no type
  specifier for the value is given. If the value has the type specifier
  @code{vec2-t} the point is initialized with the @fun{point-init-from-vec2}
  function.
  @begin[Note]{dictionary}
    The memory is allocated with the @fun{point-alloc} function and released
    with the @fun{point-free} function.
  @end{dictionary}
  @begin[Examples]{dictionary}
    Initialize a point with no value and two single float values.
    @begin{pre}
(with-graphene-point (p)
  (list (point-x p) (point-y p)))
=> (0.0 0.0)
(with-graphene-point (p 1.5 1.7)
  (list (point-x p) (point-y p)))
=> (1.5 1.7)
    @end{pre}
    Use a vector for initialization of the point.
    @begin{pre}
(with-graphene-vec2 (v 3.5 4.5)
  (with-graphene-point (p (v vec2-t))
    (list (point-x p) (point-y p))))
=> (3.5 4.5)
    @end{pre}
    This examples uses the @fun{with-graphene-points} macro to initialize
    two points. The second point is intialized with the values from the
    first point.
    @begin{pre}
(with-graphene-points ((p1 0.3 0.5) (p2 p1))
  (list (point-x p2) (point-y p2)))
=> (0.3 0.5)
    @end{pre}
  @end{dictionary}
  @see-symbol{point-t}
  @see-symbol{vec2-t}
  @see-macro{with-graphene-points}
  @see-function{point-alloc}
  @see-function{point-free}"
  (cond ((not args)
         ;; We have no arguments, the default is initialization with zeros.
         `(let ((,var (point-alloc)))
            (point-init ,var 0.0 0.0)
            (unwind-protect
              (progn ,@body)
              (point-free ,var))))
        ((not (second args))
         ;; We have one argument. The argument must be of type point-t or vec2-t
         (destructuring-bind (arg &optional type)
             (if (listp (first args)) (first args) (list (first args)))
           (cond ((or (not type)
                      (eq type 'point-t))
                  ;; One argument with no type or of type point-t
                  `(let ((,var (point-alloc)))
                     (point-init-from-point ,var ,arg)
                     (unwind-protect
                       (progn ,@body)
                       (point-free ,var))))
                 ((eq type 'vec2-t)
                  ;; One argument of type vec2-t
                  `(let ((,var (point-alloc)))
                     (point-init-from-vec2 ,var ,arg)
                     (unwind-protect
                       (progn ,@body)
                       (point-free ,var))))
                 (t
                  (error "Type error in WITH-GRAPHENE-POINT")))))
        ((not (third args))
         ;; We have a list of two arguments with (x,y) values
         `(let ((,var (point-alloc)))
            (point-init ,var ,@args)
            (unwind-protect
              (progn ,@body)
              (point-free ,var))))
        (t
         (error "Syntax error in WITH-GRAPHENE-POINT"))))

(export 'with-graphene-point)

(defmacro with-graphene-points (vars &body body)
 #+liber-documentation
 "@version{#2022-9-29}
  @syntax[]{with-graphene-points (p1 p2 p3 ... pn) body => result}
  @argument[p1 ... pn]{the newly created @symbol{point-t} instances}
  @argument[body]{a body that uses the bindings @arg{p1} ... @arg{pn}}
  @begin{short}
    The @sym{with-graphene-points} macro creates new variable bindings and
    executes the body that use these bindings.
  @end{short}
  The macro performs the bindings sequentially, like the @sym{let*} macro.

  Each point can be initialized with values using the syntax for the
  @fun{with-graphene-point} macro. See also the @fun{within-graphene-point}
  documentation.
  @begin[Examples]{dictionary}
    @begin{pre}
(with-graphene-points (p1 (p2 1.2 1.3) (p3 p2))
  (list (list (point-x p1) (point-y p1))
        (list (point-x p2) (point-y p2))
        (list (point-x p3) (point-y p3))))
=> ((0.0 0.0) (1.2 1.3) (1.2 1.3))
    @end{pre}
  @end{dictionary}
  @see-symbol{point-t}
  @see-macro{with-graphene-point}"
  (if vars
      (let ((var (if (listp (first vars)) (first vars) (list (first vars)))))
        `(with-graphene-point ,var
           (with-graphene-points ,(rest vars)
             ,@body)))
      `(progn ,@body)))

(export 'with-graphene-points)

;;; ----------------------------------------------------------------------------
;;; graphene_point_t
;;; ----------------------------------------------------------------------------

(cffi:defcstruct point-t
  (x :float)
  (y :float))

#+liber-documentation
(setf (liber:alias-for-symbol 'point-t)
      "CStruct"
      (liber:symbol-documentation 'point-t)
 "@version{2023-7-22}
  @begin{short}
    The @sym{graphene:point-t} structure is a data structure capable of 
    describing a point with two coordinates x and y of type single float.
  @end{short}
  @begin{pre}
(cffi:defcstruct point-t
  (x :float)
  (y :float))
  @end{pre}
  Access the coordinates with the @fun{graphene:point-x} and 
  @fun{graphene:point-y} functions.
  @see-symbol{graphene:point3d-t}
  @see-function{graphene:point-x}
  @see-function{graphene:point-y}")

(export 'point-t)

;;; --- Acessor Implementations ------------------------------------------------

;;;     point-x

(defun point-x (point)
  (cffi:foreign-slot-value point '(:struct point-t) 'x))

(defun (setf point-x) (value point)
  (setf (cffi:foreign-slot-value point '(:struct point-t) 'x) value))

#+liber-documentation
(setf (liber:alias-for-function 'point-x)
      "Accessor"
      (documentation 'point-x 'function)
 "@version{2023-7-22}
  @syntax[]{(graphene:point-x point) => x}
  @syntax[]{(setf (graphene:point-x point) x)}
  @argument[point]{a @symbol{graphene:point-t} instance}
  @argument[x]{a single float with the x coordinate}
  @begin{short}
    Accessor of the @code{x} slot of the @symbol{graphene:point-t} structure.
  @end{short}
  @begin[Examples]{dictionary}
    @begin{pre}
(graphene:with-graphene-point (p 0.5 1.0) (graphene:point-x p))
=> 0.5
(graphene:with-graphene-point (p) (setf (graphene:point-x p) 2.0))
=> 2.0
  @end{pre}
  @end{dictionary}
  @see-symbol{graphene:point-t}")

(export 'point-x)

;;; --- point-y ----------------------------------------------------------------

(defun point-y (point)
  (cffi:foreign-slot-value point '(:struct point-t) 'y))

(defun (setf point-y) (value point)
  (setf (cffi:foreign-slot-value point '(:struct point-t) 'y) value))

#+liber-documentation
(setf (liber:alias-for-function 'point-y)
      "Accessor"
      (documentation 'point-y 'function)
 "@version{2023-7-22}
  @syntax[]{(graphene:point-y point) => y}
  @syntax[]{(setf (graphene:point-y point) y)}
  @argument[point]{a @symbol{graphene:point-t} instance}
  @argument[y]{a single float with the y coordinate}
  @begin{short}
    Accessor of the @code{y} slot of the @symbol{graphene:point-t} structure.
  @end{short}
  @begin[Examples]{dictionary}
    @begin{pre}
(graphene:with-graphene-point (p 0.5 1.0) (graphene:point-y p))
=> 1.0
(graphene:with-graphene-point (p) (setf (graphene:point-y p) 2.0))
=> 2.0
  @end{pre}
  @end{dictionary}
  @see-symbol{graphene:point-t}")

(export 'point-y)

;;; ----------------------------------------------------------------------------
;;; graphene_point_alloc ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_point_alloc" point-alloc) (:pointer (:struct point-t))
 #+liber-documentation
 "@version{#2022-9-17}
  @return{The newly allocated @symbol{point-t} instance.}
  @begin{short}
    Allocates a new @symbol{point-t} instance.
  @end{short}
  The coordinates of the returned point are initialized to (0.0, 0.0). Use the
  @fun{point-free} function to free the resources allocated by this function.
  @begin[Examples]{dictionary}
    It is possible to chain this function with the @fun{point-init} or
    @fun{point-init-from-point} functions.
    @begin{pre}
(defun point-new (x y)
  (point-init (point-alloc) x y))
(defun point-copy (p)
  (point-init-from-point (point-alloc) p))
    @end{pre}
  @end{dictionary}
  @see-symbol{point-t}
  @see-function{point-free}
  @see-function{point-init}
  @see-function{point-init-from-point}")

(export 'point-alloc)

;;; ----------------------------------------------------------------------------
;;; graphene_point_free ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_point_free" point-free) :void
 #+liber-documentation
 "@version{#2022-9-17}
  @argument[point]{a @symbol{point-t} instance}
  @begin{short}
    Frees the resources allocated by the @fun{point-alloc} function.
  @end{short}
  @see-symbol{point-t}
  @see-function{point-alloc}"
  (point (:pointer (:struct point-t))))

(export 'point-free)

;;; ----------------------------------------------------------------------------
;;; graphene_point_zero ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_point_zero" point-zero)
    (:pointer (:struct point-t))
 #+liber-documentation
 "@version{#2022-9-19}
  @return{The @symbol{point-t} instance with a zero point.}
  @begin{short}
    Returns a point with all two coordiantes set to zero.
  @end{short}
  @begin[Examples]{dictionary}
    @begin{pre}
(values (point-x (point-zero)) (point-y (point-zero)))
=> 0.0
=> 0.0
    @end{pre}
  @end{dictionary}
  @see-symbol{point-t}")

(export 'point-zero)

;;; ----------------------------------------------------------------------------
;;; graphene_point_init ()
;;; ----------------------------------------------------------------------------

(defun point-init (point x y)
 #+liber-documentation
 "@version{#2022-9-19}
  @argument[point]{a @symbol{point-t} instance}
  @argument[x]{a number coerced to a single float with the x coordinate}
  @argument[y]{a number coerced to a single float with the y coordinate}
  @return{The initialized @symbol{point-t} instance.}
  @begin{short}
    Initializes the point to the given @arg{x} and @arg{y} coordinates.
  @end{short}
  It is safe to call this function multiple times.
  @see-symbol{point-t}"
  (cffi:foreign-funcall "graphene_point_init"
                        (:pointer (:struct point-t)) point
                        :float (coerce x 'single-float)
                        :float (coerce y 'single-float)
                        (:pointer (:struct point-t))))

(export 'point-init)

;;; ----------------------------------------------------------------------------
;;; graphene_point_init_from_point ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_point_init_from_point" point-init-from-point)
    (:pointer (:struct point-t))
 #+liber-documentation
 "@version{#2022-9-19}
  @argument[point]{a @symbol{point-t} instance}
  @argument[source]{a @symbol{point-t} instance to use}
  @return{The initialized @symbol{point-t} instance.}
  @short{Initializes the point using the coordinates of @arg{source}.}
  @see-symbol{point-t}"
  (point (:pointer (:struct point-t)))
  (source (:pointer (:struct point-t))))

(export 'point-init-from-point)

;;; ----------------------------------------------------------------------------
;;; graphene_point_init_from_vec2 ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_point_init_from_vec2" point-init-from-vec2)
   (:pointer (:struct point-t))
 #+liber-documentation
 "@version{#2022-9-19}
  @argument[point]{a @symbol{point-t} instance}
  @argument[vector]{a @symbol{vec2-t} vector to use}
  @return{The initialized @symbol{point-t} instance.}
  @short{Initializes the point using the components of the given vector.}
  @see-symbol{point-t}
  @see-symbol{vec2-t}"
  (point (:pointer (:struct point-t)))
  (vector :pointer)) ; vec2-t not known at this point

(export 'point-init-from-vec2)

;;; ----------------------------------------------------------------------------
;;; graphene_point_to_vec2 ()
;;; ----------------------------------------------------------------------------

(defun point-to-vec2 (point vector)
 #+liber-documentation
 "@version{#2022-9-19}
  @argument[point]{a @symbol{point-t} instance}
  @argument[vector]{a @symbol{vec2-t} instance}
  @return{The @symbol{vec2-t} instance with the coordinates of the point.}
  @short{Stores the coordinates of the given point into a vector.}
  @begin[Examples]{dictionary}
    @begin{pre}
(with-graphene-point (p 1.0 2.0)
  (with-graphene-vec2 (v)
    (vec2-to-float (point-to-vec2 p v))))
=> (1.0 2.0)
    @end{pre}
  @end{dictionary}
  @see-symbol{point-t}
  @see-symbol{vec2-t}"
  (cffi:foreign-funcall "graphene_point_to_vec2"
                        (:pointer (:struct point-t)) point
                        :pointer vector ; vec2-t not known at this point
                        :void)
  vector)

(export 'point-to-vec2)

;;; ----------------------------------------------------------------------------
;;; graphene_point_equal ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_point_equal" point-equal) :bool
 #+liber-documentation
 "@version{#2022-9-19}
  @argument[a]{a @symbol{point-t} instance}
  @argument[b]{a @symbol{point-t} instance}
  @return{@em{True}, if the points have the same coordinates, otherwise
    @em{false}.}
  @begin{short}
    Checks whether two given points are equal.
  @end{short}
  This function accounts for floating point fluctuations. If you want to
  control the fuzziness of the match, you can use the @fun{point-near} function
  instead.
  @see-symbol{point-t}
  @see-function{point-near}"
  (a (:pointer (:struct point-t)))
  (b (:pointer (:struct point-t))))

(export 'point-equal)

;;; ----------------------------------------------------------------------------
;;; graphene_point_near ()
;;; ----------------------------------------------------------------------------

(defun point-near (a b epsilon)
 #+liber-documentation
 "@version{#2022-9-19}
  @argument[a]{a @symbol{point-t} instance}
  @argument[b]{a @symbol{point-t} instance}
  @argument[epsilon]{a number coerced to a single float with the threshold
    between the two points}
  @return{@em{True}, if the distance between the points is within
    @arg{epsilon}.}
  @begin{short}
    Checks whether the two points @arg{a} and @arg{b} are within the threshold
    of @arg{epsilon} equal.
  @end{short}
  @see-symbol{point-t}
  @see-function{point-equal}"
  (cffi:foreign-funcall "graphene_point_near"
                        (:pointer (:struct point-t)) a
                        (:pointer (:struct point-t)) b
                        :float (coerce epsilon 'single-float)
                        :bool))

(export 'point-near)

;;; ----------------------------------------------------------------------------
;;; graphene_point_distance ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_point_distance" %point-distance) :float
  (a (:pointer (:struct point-t)))
  (b (:pointer (:struct point-t)))
  (dx (:pointer :float))
  (dy (:pointer :float)))

(defun point-distance (a b)
 #+liber-documentation
 "@version{#2022-9-19}
  @syntax[]{(point-distance a b) => distance, dx, dy}
  @argument[a]{a @symbol{point-t} instance}
  @argument[b]{a @symbol{point-t} instance}
  @argument[distance]{a single float with the distance between the two points}
  @argument[dx]{a single float with the distance component of the x axis}
  @argument[dy]{a single float with the distance component of the y axis}
  @short{Computes the distance between the two given points.}
  @see-symbol{point-t}"
  (cffi:with-foreign-objects ((dx :float) (dy :float))
    (values (%point-distance a b dx dy)
            (cffi:mem-ref dx :float)
            (cffi:mem-ref dy :float))))

(export 'point-distance)

;;; ----------------------------------------------------------------------------
;;; graphene_point_interpolate ()
;;; ----------------------------------------------------------------------------

(defun point-interpolate (a b factor result)
 #+liber-documentation
 "@version{#2022-9-19}
  @argument[a]{a @symbol{point-t} instance}
  @argument[b]{a @symbol{point-t} instance}
  @argument[factor]{a number coerced to a double float with the linear
    interpolation factor}
  @argument[result]{a @symbol{point-t} instance for the interpolated point}
  @return{The @symbol{point-t} instance with the interpolated point.}
  @begin{short}
    Linearly interpolates the coordinates of @arg{a} and @arg{b} using the
    given @arg{factor}.
  @end{short}
  @begin[Examples]{dictionary}
    @begin{pre}
(with-graphene-points ((a 0 0) (b 1 2) result)
  (point-interpolate a b 0.5 result)
  (values (point-x result) (point-y result)))
=> 0.5
=> 1.0
    @end{pre}
  @end{dictionary}
  @see-symbol{point-t}"
  (cffi:foreign-funcall "graphene_point_interpolate"
                        (:pointer (:struct point-t)) a
                        (:pointer (:struct point-t)) b
                        :double (coerce factor 'double-float)
                        (:pointer (:struct point-t)) result
                        :void)
  result)

(export 'point-interpolate)

;;; --- End of file graphene.point.lisp ----------------------------------------
