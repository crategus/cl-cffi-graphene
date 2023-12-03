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

(defmacro with-point ((var &rest args) &body body)
 #+liber-documentation
 "@version{2023-12-2}
  @syntax[]{(grahene:with-point (p) body) => result}
  @syntax[]{(graphene:with-point (p x y) body) => result}
  @syntax[]{(graphene:with-point (p p1) body) => result}
  @syntax[]{(graphene:with-point (p (v graphene:vec2-t)) body) => result}
  @argument[p]{a @symbol{graphene:point-t} instance to create and initialize}
  @argument[x]{a number coerced to a float for the x component of the point}
  @argument[y]{a number coerced to a float for the y component of the point}
  @argument[p1]{a @symbol{graphene:point-t} instance to use for initialization}
  @argument[v]{a @symbol{graphene:vec2-t} instance to use for initialization}
  @begin{short}
    The @fun{graphene:with-point} macro allocates a new
    @symbol{graphene:point-t} instance, initializes the point with the given
    values and executes the body that uses the point.
  @end{short}
  After execution of the body the allocated memory for the point is released.

  When no argument is given the components of the point are initialized to zero.
  The initialization with two float values uses the @fun{graphene:point-init}
  function. The initialization from another point is done with the
  @fun{graphene:point-init-from-point} function. That is the default when no
  type specifier for the value is given. If the value has the type specifier
  @code{graphene:vec2-t} the point is initialized with the
  @fun{graphene:point-init-from-vec2} function.
  @begin[Note]{dictionary}
    The memory is allocated with the @fun{graphene:point-alloc} function and
    released with the @fun{graphene:point-free} function.
  @end{dictionary}
  @begin[Examples]{dictionary}
    Initialize a point with no value and two single float values.
    @begin{pre}
(graphene:with-point (p)
  (list (graphene:point-x p) (graphene:point-y p)))
=> (0.0 0.0)
(graphene:with-point (p 1.5 1.7)
  (list (graphene:point-x p) (graphene:point-y p)))
=> (1.5 1.7)
    @end{pre}
    Use a vector for initialization of the point.
    @begin{pre}
(graphene:with-vec2 (v 3.5 4.5)
  (graphene:with-point (p (v graphene:vec2-t))
    (list (graphene:point-x p) (graphene:point-y p))))
=> (3.5 4.5)
    @end{pre}
    This examples uses the @fun{graphene:with-points} macro to initialize two
    points. The second point is intialized with the values from the first point.
    @begin{pre}
(grapene:with-points ((p1 0.3 0.5) (p2 p1))
  (list (graphene:point-x p2) (graphene:point-y p2)))
=> (0.3 0.5)
    @end{pre}
  @end{dictionary}
  @see-symbol{graphene:point-t}
  @see-symbol{graphene:vec2-t}
  @see-macro{graphene:with-points}
  @see-function{graphene:point-alloc}
  @see-function{graphene:point-free}"
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
                  (error "Type error in GRAPHENE:WITH-POINT")))))
        ((not (third args))
         ;; We have a list of two arguments with (x,y) values
         `(let ((,var (point-alloc)))
            (point-init ,var ,@args)
            (unwind-protect
              (progn ,@body)
              (point-free ,var))))
        (t
         (error "Syntax error in GRAPHENE:WITH-POINT"))))

(export 'with-point)

(defmacro with-points (vars &body body)
 #+liber-documentation
 "@version{2023-12-2}
  @syntax[]{(graphene:with-points (p1 p2 p3 ... pn) body) => result}
  @argument[p1 ... pn]{the newly created @symbol{graphene:point-t} instances}
  @argument[body]{a body that uses the bindings @arg{p1 ... pn}}
  @begin{short}
    The @fun{graphene:with-points} macro creates new variable bindings and
    executes the body that use these bindings.
  @end{short}
  The macro performs the bindings sequentially, like the @sym{let*} macro.

  Each point can be initialized with values using the syntax for the
  @fun{graphene:with-point} macro. See also the @fun{graphene:with-point}
  documentation.
  @begin[Examples]{dictionary}
    @begin{pre}
(graphene:with-points (p1 (p2 1.2 1.3) (p3 p2))
  (list (list (graphene:point-x p1) (graphene:point-y p1))
        (list (graphene:point-x p2) (graphene:point-y p2))
        (list (graphene:point-x p3) (graphene:point-y p3))))
=> ((0.0 0.0) (1.2 1.3) (1.2 1.3))
    @end{pre}
  @end{dictionary}
  @see-symbol{graphene:point-t}
  @see-macro{graphene:with-point}"
  (if vars
      (let ((var (if (listp (first vars)) (first vars) (list (first vars)))))
        `(with-point ,var
           (with-points ,(rest vars)
             ,@body)))
      `(progn ,@body)))

(export 'with-points)

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
    The @symbol{graphene:point-t} structure is a data structure capable of
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

;;; --- Accessor Implementations -----------------------------------------------

;;; --- point-x ----------------------------------------------------------------

(defun point-x (point)
  (cffi:foreign-slot-value point '(:struct point-t) 'x))

(defun (setf point-x) (value point)
  (setf (cffi:foreign-slot-value point '(:struct point-t) 'x)
        (coerce value 'single-float)))

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
(graphene:with-point (p 0.5 1.0) (graphene:point-x p))
=> 0.5
(graphene:with-point (p) (setf (graphene:point-x p) 2.0))
=> 2.0
  @end{pre}
  @end{dictionary}
  @see-symbol{graphene:point-t}")

(export 'point-x)

;;; --- point-y ----------------------------------------------------------------

(defun point-y (point)
  (cffi:foreign-slot-value point '(:struct point-t) 'y))

(defun (setf point-y) (value point)
  (setf (cffi:foreign-slot-value point '(:struct point-t) 'y)
        (coerce value 'single-float)))

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
(graphene:with-point (p 0.5 1.0) (graphene:point-y p))
=> 1.0
(graphene:with-point (p) (setf (graphene:point-y p) 2.0))
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
 "@version{2023-9-22}
  @return{The newly allocated @symbol{graphene:point-t} instance.}
  @begin{short}
    Allocates a new @symbol{graphene:point-t} instance.
  @end{short}
  The coordinates of the returned point are initialized to (0.0, 0.0). Use the
  @fun{graphene:point-free} function to free the resources allocated by this
  function.
  @begin[Examples]{dictionary}
    It is possible to chain this function with the @fun{graphene:point-init} or
    @fun{graphene:point-init-from-point} functions.
    @begin{pre}
(defun point-new (x y)
  (graphene:point-init (graphene:point-alloc) x y))
(defun point-copy (p)
  (graphene:point-init-from-point (graphene:point-alloc) p))
    @end{pre}
  @end{dictionary}
  @see-symbol{graphene:point-t}
  @see-function{graphene:point-free}
  @see-function{graphene:point-init}
  @see-function{graphene:point-init-from-point}")

(export 'point-alloc)

;;; ----------------------------------------------------------------------------
;;; graphene_point_free ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_point_free" point-free) :void
 #+liber-documentation
 "@version{2023-9-22}
  @argument[point]{a @symbol{graphene:point-t} instance}
  @begin{short}
    Frees the resources allocated by the @fun{graphene:point-alloc} function.
  @end{short}
  @see-symbol{graphene:point-t}
  @see-function{graphene:point-alloc}"
  (point (:pointer (:struct point-t))))

(export 'point-free)

;;; ----------------------------------------------------------------------------
;;; graphene_point_zero ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_point_zero" point-zero) (:pointer (:struct point-t))
 #+liber-documentation
 "@version{2023-9-22}
  @return{The @symbol{graphene:point-t} instance with a zero point.}
  @begin{short}
    Returns a point with all two coordiantes set to zero.
  @end{short}
  @begin[Examples]{dictionary}
    @begin{pre}
(values (graphene:point-x (graphene:point-zero))
        (graphene:point-y (graphene:point-zero)))
=> 0.0
=> 0.0
    @end{pre}
  @end{dictionary}
  @see-symbol{graphene:point-t}")

(export 'point-zero)

;;; ----------------------------------------------------------------------------
;;; graphene_point_init ()
;;; ----------------------------------------------------------------------------

(defun point-init (point x y)
 #+liber-documentation
 "@version{2023-9-22}
  @argument[point]{a @symbol{graphene:point-t} instance}
  @argument[x]{a number coerced to a single float with the x coordinate}
  @argument[y]{a number coerced to a single float with the y coordinate}
  @return{The initialized @symbol{graphene:point-t} instance.}
  @begin{short}
    Initializes the point to the given @arg{x} and @arg{y} coordinates.
  @end{short}
  It is safe to call this function multiple times.
  @see-symbol{graphene:point-t}"
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
 "@version{2023-9-22}
  @argument[point]{a @symbol{graphene:point-t} instance}
  @argument[source]{a @symbol{graphene:point-t} instance to use}
  @return{The initialized @symbol{graphene:point-t} instance.}
  @short{Initializes the point using the coordinates of @arg{source}.}
  @see-symbol{graphene:point-t}"
  (point (:pointer (:struct point-t)))
  (source (:pointer (:struct point-t))))

(export 'point-init-from-point)

;;; ----------------------------------------------------------------------------
;;; graphene_point_init_from_vec2 ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_point_init_from_vec2" point-init-from-vec2)
   (:pointer (:struct point-t))
 #+liber-documentation
 "@version{2023-9-22}
  @argument[point]{a @symbol{graphene:point-t} instance}
  @argument[vector]{a @symbol{graphene:vec2-t} vector to use}
  @return{The initialized @symbol{graphene:point-t} instance.}
  @short{Initializes the point using the components of the given vector.}
  @see-symbol{graphene:point-t}
  @see-symbol{graphene:vec2-t}"
  (point (:pointer (:struct point-t)))
  (vector :pointer)) ; vec2-t not known at this point

(export 'point-init-from-vec2)

;;; ----------------------------------------------------------------------------
;;; graphene_point_to_vec2 ()
;;; ----------------------------------------------------------------------------

(defun point-to-vec2 (point vector)
 #+liber-documentation
 "@version{2023-9-22}
  @argument[point]{a @symbol{graphene:point-t} instance}
  @argument[vector]{a @symbol{graphene:vec2-t} instance}
  @return{The @symbol{graphene:vec2-t} instance with the coordinates of the
    point.}
  @short{Stores the coordinates of the given point into a vector.}
  @begin[Examples]{dictionary}
    @begin{pre}
(graphene:with-point (p 1.0 2.0)
  (graphene:with-vec2 (v)
    (graphene:vec2-to-float (graphene:point-to-vec2 p v))))
=> (1.0 2.0)
    @end{pre}
  @end{dictionary}
  @see-symbol{graphene:point-t}
  @see-symbol{graphene:vec2-t}"
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
 "@version{2023-9-22}
  @argument[a]{a @symbol{graphene:point-t} instance}
  @argument[b]{a @symbol{graphene:point-t} instance}
  @return{@em{True}, if the points have the same coordinates, otherwise
    @em{false}.}
  @begin{short}
    Checks whether two given points are equal.
  @end{short}
  This function accounts for floating point fluctuations. If you want to
  control the fuzziness of the match, you can use the @fun{graphene:point-near}
  function instead.
  @see-symbol{graphene:point-t}
  @see-function{graphene:point-near}"
  (a (:pointer (:struct point-t)))
  (b (:pointer (:struct point-t))))

(export 'point-equal)

;;; ----------------------------------------------------------------------------
;;; graphene_point_near ()
;;; ----------------------------------------------------------------------------

(defun point-near (a b epsilon)
 #+liber-documentation
 "@version{2023-9-22}
  @argument[a]{a @symbol{graphene:point-t} instance}
  @argument[b]{a @symbol{graphene:point-t} instance}
  @argument[epsilon]{a number coerced to a single float with the threshold
    between the two points}
  @return{@em{True}, if the distance between the points is within
    @arg{epsilon}.}
  @begin{short}
    Checks whether the two points @arg{a} and @arg{b} are within the threshold
    of @arg{epsilon} equal.
  @end{short}
  @see-symbol{graphene:point-t}
  @see-function{graphene:point-equal}"
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
 "@version{2023-9-22}
  @syntax[]{(graphene:point-distance a b) => distance, dx, dy}
  @argument[a]{a @symbol{graphene:point-t} instance}
  @argument[b]{a @symbol{graphene:point-t} instance}
  @argument[distance]{a single float with the distance between the two points}
  @argument[dx]{a single float with the distance component of the x axis}
  @argument[dy]{a single float with the distance component of the y axis}
  @short{Computes the distance between the two given points.}
  @see-symbol{graphene:point-t}"
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
 "@version{2023-9-22}
  @argument[a]{a @symbol{graphene:point-t} instance}
  @argument[b]{a @symbol{graphene:point-t} instance}
  @argument[factor]{a number coerced to a double float with the linear
    interpolation factor}
  @argument[result]{a @symbol{graphene:point-t} instance for the interpolated
    point}
  @return{The @symbol{graphene:point-t} instance with the interpolated point.}
  @begin{short}
    Linearly interpolates the coordinates of @arg{a} and @arg{b} using the
    given @arg{factor}.
  @end{short}
  @begin[Examples]{dictionary}
    @begin{pre}
(graphene:with-points ((a 0 0) (b 1 2) result)
  (graphene:point-interpolate a b 0.5 result)
  (values (graphene:point-x result) (graphene:point-y result)))
=> 0.5
=> 1.0
    @end{pre}
  @end{dictionary}
  @see-symbol{graphene:point-t}"
  (cffi:foreign-funcall "graphene_point_interpolate"
                        (:pointer (:struct point-t)) a
                        (:pointer (:struct point-t)) b
                        :double (coerce factor 'double-float)
                        (:pointer (:struct point-t)) result
                        :void)
  result)

(export 'point-interpolate)

;;; --- End of file graphene.point.lisp ----------------------------------------
