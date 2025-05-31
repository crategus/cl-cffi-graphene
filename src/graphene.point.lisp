;;; ----------------------------------------------------------------------------
;;; graphene.point.lisp
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
 "@version{2025-05-09}
  @syntax{(graphene:with-point (p) body) => result}
  @syntax{(graphene:with-point (p p1) body) => result}
  @syntax{(graphene:with-point (p (v graphene:vec2-t)) body) => result}
  @syntax{(graphene:with-point (p x y) body) => result}
  @argument[p]{a @symbol{graphene:point-t} instance to create and initialize}
  @argument[p1]{a @symbol{graphene:point-t} instance to use for initialization}
  @argument[v]{a @symbol{graphene:vec2-t} instance to use for initialization}
  @argument[x]{a number coerced to a single float for the x component}
  @argument[y]{a number coerced to a single float for the y component}
  @begin{short}
    The @fun{graphene:with-point} macro allocates a new
    @symbol{graphene:point-t} instance, initializes the point with the given
    values and executes the body that uses the point.
  @end{short}
  After execution of the body the allocated memory for the point is released.

  If no argument is given, the components of the point are initialized to zero.
  The initialization with two single floats uses the @fun{graphene:point-init}
  function. The initialization from another point is done with the
  @fun{graphene:point-init-from-point} function. That is the default when no
  type specifier for the value is given. If the value has the type specifier
  @code{graphene:vec2-t} the point is initialized with the
  @fun{graphene:point-init-from-vec2} function.
  @begin[Notes]{dictionary}
    The memory is allocated with the @fun{graphene:point-alloc} function and
    released with the @fun{graphene:point-free} function.
  @end{dictionary}
  @begin[Examples]{dictionary}
    Initialize a point with no value and two single floats.
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
  @end{dictionary}
  @see-symbol{graphene:point-t}
  @see-symbol{graphene:vec2-t}
  @see-macro{graphene:with-points}
  @see-function{graphene:point-alloc}
  @see-function{graphene:point-free}"
  (cond ((null args)
         ;; No arguments, the default is initialization with zeros
         `(let ((,var (point-alloc)))
            (point-init ,var 0.0 0.0)
            (unwind-protect
              (progn ,@body)
              (point-free ,var))))
        ((null (second args))
         ;; One argument of type point-t or vec2-t
         (dbind (arg1 &optional type1 &rest rest1) (mklist (first args))
           (declare (ignore rest1))
           (cond ((eq type1 'point-t)
                  ;; One argument of type point-t
                  `(let ((,var (point-alloc)))
                     (point-init-from-point ,var ,arg1)
                     (unwind-protect
                       (progn ,@body)
                       (point-free ,var))))
                 ((eq type1 'vec2-t)
                  ;; One argument of type vec2-t
                  `(let ((,var (point-alloc)))
                     (point-init-from-vec2 ,var ,arg1)
                     (unwind-protect
                       (progn ,@body)
                       (point-free ,var))))
                 (t
                  ;; One argument with no type, default is point-t
                  `(let ((,var (point-alloc)))
                     (point-init-from-point ,var ,@args)
                     (unwind-protect
                       (progn ,@body)
                       (point-free ,var)))))))
        ((not (third args))
         ;; Two arguments for the x,y components
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
 "@version{2025-4-1}
  @syntax{(graphene:with-points (p1 p2 p3 ... pn) body) => result}
  @argument[p1 ... pn]{newly created @symbol{graphene:point-t} instances}
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
      (let ((var (mklist (first vars))))
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
 "@version{2025-4-1}
  @begin{declaration}
(cffi:defcstruct point-t
  (x :float)
  (y :float))
  @end{declaration}
  @begin{short}
    The @symbol{graphene:point-t} structure is a data structure capable of
    describing a point with two coordinates x and y of type single float.
  @end{short}

  Access the coordinates with the @fun{graphene:point-x} and
  @fun{graphene:point-y} functions. Use the @macro{graphene:with-point} and
  @macro{graphene:with-points} macros to allocate a new
  @symbol{graphene:point-t} instance and initialize the point with values.
  @begin[Examples]{dictionary}
    Allocate and initialize a point with values.
    @begin{pre}
(graphene:with-point (p 1.0 1.5)
  (list (graphene:point-x p)
        (graphene:point-y p)))
=> (1.0 1.5)
    @end{pre}
  @end{dictionary}
  @see-slot{graphene:point-x}
  @see-slot{graphene:point-y}
  @see-symbol{graphene:point3d-t}
  @see-macro{graphene:with-point}
  @see-macro{graphene:with-points}")

(export 'point-t)

;;; --- graphene:point-x -------------------------------------------------------

(defun point-x (p)
  (cffi:foreign-slot-value p '(:struct point-t) 'x))

(defun (setf point-x) (value p)
  (setf (cffi:foreign-slot-value p '(:struct point-t) 'x)
        (coerce value 'single-float)))

#+liber-documentation
(setf (liber:alias-for-function 'point-x)
      "Accessor"
      (documentation 'point-x 'function)
 "@version{2025-4-4}
  @syntax{(graphene:point-x p) => x}
  @syntax{(setf (graphene:point-x p) x)}
  @argument[p]{a @symbol{graphene:point-t} instance}
  @argument[x]{a number coerced to a single float for the x coordinate}
  @begin{short}
    Accessor of the @code{x} slot of the @symbol{graphene:point-t} structure.
  @end{short}
  The @arg{x} value is coerced to a single float before assignment.
  @begin[Examples]{dictionary}
    @begin{pre}
(graphene:with-point (p 0.5 1.0) (graphene:point-x p))
=> 0.5
(graphene:with-point (p) (setf (graphene:point-x p) 3/2))
=> 1.5
  @end{pre}
  @end{dictionary}
  @see-symbol{graphene:point-t}")

(export 'point-x)

;;; --- graphene:point-y -------------------------------------------------------

(defun point-y (p)
  (cffi:foreign-slot-value p '(:struct point-t) 'y))

(defun (setf point-y) (value p)
  (setf (cffi:foreign-slot-value p '(:struct point-t) 'y)
        (coerce value 'single-float)))

#+liber-documentation
(setf (liber:alias-for-function 'point-y)
      "Accessor"
      (documentation 'point-y 'function)
 "@version{2025-4-4}
  @syntax{(graphene:point-y p) => y}
  @syntax{(setf (graphene:point-y p) y)}
  @argument[p]{a @symbol{graphene:point-t} instance}
  @argument[y]{a number coerced to a single float for the y coordinate}
  @begin{short}
    Accessor of the @code{y} slot of the @symbol{graphene:point-t} structure.
  @end{short}
  The @arg{y} value is coerced to a single float before assignment.
  @begin[Examples]{dictionary}
    @begin{pre}
(graphene:with-point (p 0.5 1.0) (graphene:point-y p))
=> 1.0
(graphene:with-point (p) (setf (graphene:point-y p) 3/2))
=> 1.5
  @end{pre}
  @end{dictionary}
  @see-symbol{graphene:point-t}")

(export 'point-y)

;;; ----------------------------------------------------------------------------
;;; graphene_point_alloc
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_point_alloc" point-alloc) (:pointer (:struct point-t))
 #+liber-documentation
 "@version{2025-4-1}
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
;;; graphene_point_free
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_point_free" point-free) :void
 #+liber-documentation
 "@version{2025-4-1}
  @argument[p]{a @symbol{graphene:point-t} instance}
  @begin{short}
    Frees the resources allocated by the @fun{graphene:point-alloc} function.
  @end{short}
  @see-symbol{graphene:point-t}
  @see-function{graphene:point-alloc}"
  (p (:pointer (:struct point-t))))

(export 'point-free)

;;; ----------------------------------------------------------------------------
;;; graphene_point_zero
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_point_zero" point-zero) (:pointer (:struct point-t))
 #+liber-documentation
 "@version{2025-4-1}
  @return{The @symbol{graphene:point-t} instance with a zero point.}
  @begin{short}
    Returns a point with the two coordinates set to zero.
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
;;; graphene_point_init
;;; ----------------------------------------------------------------------------

(defun point-init (p x y)
 #+liber-documentation
 "@version{2025-4-1}
  @argument[p]{a @symbol{graphene:point-t} instance}
  @argument[x]{a number coerced to a single float for the x coordinate}
  @argument[y]{a number coerced to a single float for the y coordinate}
  @return{The initialized @symbol{graphene:point-t} instance.}
  @begin{short}
    Initializes the point to the given @arg{x} and @arg{y} coordinates.
  @end{short}
  It is safe to call this function multiple times.
  @see-symbol{graphene:point-t}"
  (cffi:foreign-funcall "graphene_point_init"
                        (:pointer (:struct point-t)) p
                        :float (coerce x 'single-float)
                        :float (coerce y 'single-float)
                        (:pointer (:struct point-t))))

(export 'point-init)

;;; ----------------------------------------------------------------------------
;;; graphene_point_init_from_point
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_point_init_from_point" point-init-from-point)
    (:pointer (:struct point-t))
 #+liber-documentation
 "@version{2025-4-1}
  @argument[p]{a @symbol{graphene:point-t} instance}
  @argument[src]{a @symbol{graphene:point-t} instance to use}
  @return{The initialized @symbol{graphene:point-t} instance.}
  @short{Initializes the point using the coordinates of @arg{src}.}
  @see-symbol{graphene:point-t}"
  (p (:pointer (:struct point-t)))
  (src (:pointer (:struct point-t))))

(export 'point-init-from-point)

;;; ----------------------------------------------------------------------------
;;; graphene_point_init_from_vec2
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_point_init_from_vec2" point-init-from-vec2)
   (:pointer (:struct point-t))
 #+liber-documentation
 "@version{2025-4-1}
  @argument[p]{a @symbol{graphene:point-t} instance}
  @argument[v]{a @symbol{graphene:vec2-t} instance to use}
  @return{The initialized @symbol{graphene:point-t} instance.}
  @short{Initializes the point using the components of the given vector.}
  @see-symbol{graphene:point-t}
  @see-symbol{graphene:vec2-t}"
  (p (:pointer (:struct point-t)))
  (v :pointer)) ; vec2-t not known at this point

(export 'point-init-from-vec2)

;;; ----------------------------------------------------------------------------
;;; graphene_point_to_vec2
;;; ----------------------------------------------------------------------------

(defun point-to-vec2 (p v)
 #+liber-documentation
 "@version{2025-4-1}
  @argument[p]{a @symbol{graphene:point-t} instance}
  @argument[v]{a @symbol{graphene:vec2-t} instance}
  @begin{return}
    The @symbol{graphene:vec2-t} instance with the coordinates of the point.
  @end{return}
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
                        (:pointer (:struct point-t)) p
                        :pointer v ; vec2-t not known at this point
                        :void)
  v)

(export 'point-to-vec2)

;;; ----------------------------------------------------------------------------
;;; graphene_point_equal
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_point_equal" point-equal) :bool
 #+liber-documentation
 "@version{2025-4-1}
  @argument[a]{a @symbol{graphene:point-t} instance}
  @argument[b]{a @symbol{graphene:point-t} instance}
  @begin{return}
    @em{True}, if the points have the same coordinates, otherwise @em{false}.
  @end{return}
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
;;; graphene_point_near
;;; ----------------------------------------------------------------------------

(defun point-near (a b epsilon)
 #+liber-documentation
 "@version{2025-4-1}
  @argument[a]{a @symbol{graphene:point-t} instance}
  @argument[b]{a @symbol{graphene:point-t} instance}
  @argument[epsilon]{a number coerced to a single float with the threshold
    between the two points}
  @begin{return}
    @em{True}, if the distance between the points is within @arg{epsilon}.
  @end{return}
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
;;; graphene_point_distance
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_point_distance" %point-distance) :float
  (a (:pointer (:struct point-t)))
  (b (:pointer (:struct point-t)))
  (dx (:pointer :float))
  (dy (:pointer :float)))

(defun point-distance (a b)
 #+liber-documentation
 "@version{2025-4-1}
  @syntax{(graphene:point-distance a b) => dist, dx, dy}
  @argument[a]{a @symbol{graphene:point-t} instance}
  @argument[b]{a @symbol{graphene:point-t} instance}
  @argument[dist]{a single float for the distance between the two points}
  @argument[dx]{a single float for the distance component of the X axis}
  @argument[dy]{a single float for the distance component of the Y axis}
  @short{Computes the distance between the two given points.}
  @begin[Examples]{dictionary}
    @begin{pre}
(graphene:with-points ((a 0 0) (b 1 1))
  (graphene:point-distance a b))
=> 1.4142135
=> 1.0
=> 1.0
    @end{pre}
  @end{dictionary}
  @see-symbol{graphene:point-t}"
  (cffi:with-foreign-objects ((dx :float) (dy :float))
    (values (%point-distance a b dx dy)
            (cffi:mem-ref dx :float)
            (cffi:mem-ref dy :float))))

(export 'point-distance)

;;; ----------------------------------------------------------------------------
;;; graphene_point_interpolate
;;; ----------------------------------------------------------------------------

(defun point-interpolate (a b factor result)
 #+liber-documentation
 "@version{2025-4-1}
  @argument[a]{a @symbol{graphene:point-t} instance}
  @argument[b]{a @symbol{graphene:point-t} instance}
  @argument[factor]{a number coerced to a double float for the linear
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
