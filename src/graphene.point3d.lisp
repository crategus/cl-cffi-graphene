;;; ----------------------------------------------------------------------------
;;; graphene.point3d.lisp
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

(defmacro with-point3d ((var &rest args) &body body)
 #+liber-documentation
 "@version{2023-12-3}
  @syntax[]{(graphene:with-point3d (p) body) => result}
  @syntax[]{(graphene:with-point3d (p x y z) body) => result}
  @syntax[]{(graphene:with-point3d (p p1) body) => result}
  @syntax[]{(graphene:with-point3d (p (v graphene:vec3-t)) body) => result}
  @argument[p]{a @symbol{graphene:point3d-t} instance to create and initialize}
  @argument[x]{a number coerced to a float for the x component of the point}
  @argument[y]{a number coerced to a float for the y component of the point}
  @argument[z]{a number coerced to a float for the z component of the point}
  @argument[p1]{a @symbol{graphene:point3d-t} instance to use for
    initialization}
  @argument[v]{a @symbol{graphene:vec3-t} instance to use for initialization}
  @begin{short}
    The @fun{graphene:with-point3d} macro allocates a new
    @symbol{graphene:point3d-t} instance, initializes the point with the given
    values and executes the body that uses the point.
  @end{short}
  After execution of the body the allocated memory for the point is released.

  When no argument is given the components of the point are initialized to zero.
  The initialization with three float values uses the
  @fun{graphene:point3d-init} function. The initialization from another point
  is done with the @fun{graphene:point3d-init-from-point} function. That is the
  default when no type specifier for the value is given. If the value has the
  type specifier @code{graphene:vec3-t} the point is initialized with the
  @fun{graphene:point3d-init-from-vec3} function.
  @begin[Note]{dictionary}
    The memory is allocated with the @fun{graphene:point3d-alloc} function and
    released with the @fun{graphene:point3d-free} function.
  @end{dictionary}
  @begin[Examples]{dictionary}
    Initialize a point with no value and three float values.
    @begin{pre}
(graphene:with-point3d (p)
  (list (graphene:point3d-x p) (graphene:point3d-y p) (graphene:point3d-z p)))
=> (0.0 0.0 0.0)
(graphene:with-point3d (p 1.5 1.7 1.9)
  (list (graphene:point3d-x p) (graphene:point3d-y p) (graphene:point3d-z p)))
=> (1.5 1.7 1.9)
    @end{pre}
    Use a vector for initialization of the point.
    @begin{pre}
(graphene:with-vec3 (v 3.5 4.5 5.5)
  (graphene:with-point3d (p (v graphene:vec3-t))
    (list (graphene:point3d-x p)
          (graphene:point3d-y p)
          (graphene:point3d-z p))))
=> (3.5 4.5 5.5)
    @end{pre}
    This examples uses the @fun{graphene:with-point3ds} macro to initialize two
    points. The second point is intialized with the values from the first point.
    @begin{pre}
(graphene:with-point3ds ((p1 0.3 0.5 0.7) (p2 p1))
  (list (graphene:point3d-x p2)
        (graphene:point3d-y p2) (graphene:point3d-z p2)))
=> (0.3 0.5 0.7)
    @end{pre}
  @end{dictionary}
  @see-symbol{graphene:point3d-t}
  @see-symbol{graphene:vec3-t}
  @see-macro{graphene:with-point3ds}
  @see-function{graphene:point3d-alloc}
  @see-function{graphene:point3d-free}"
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
                  (error "Type error in GRAPHENE:WITH-POINT3D")))))
        ((not (fourth args))
         ;; We have a list of three arguments with (x,y,z) values
         `(let ((,var (point3d-alloc)))
            (point3d-init ,var ,@args)
            (unwind-protect
              (progn ,@body)
              (point3d-free ,var))))
        (t
         (error "Syntax error in GRAPHENE:WITH-POINT3D"))))

(export 'with-point3d)

(defmacro with-point3ds (vars &body body)
 #+liber-documentation
 "@version{2023-12-3}
  @syntax[]{(graphene:with-point3ds (p1 p2 p3 ... pn) body) => result}
  @argument[p1 ... pn]{the newly created @symbol{graphene:point3d-t} instances}
  @argument[body]{a body that uses the bindings @arg{p1 ... pn}}
  @begin{short}
    The @fun{graphene:with-point3ds} macro creates new variable bindings and
    executes the body that use these bindings.
  @end{short}
  The macro performs the bindings sequentially, like the @sym{let*} macro.

  Each point can be initialized with values using the syntax for the
  @fun{graphene:with-point3d} macro. See also the @fun{graphene:with-point3d}
  documentation.
  @begin[Examples]{dictionary}
    @begin{pre}
(graphene:with-point3ds (p1 (p2 1.2 1.3 1.4) (p3 p2))
  (list (list (graphene:point3d-x p1)
              (graphene:point3d-y p1) (graphene:point3d-z p1))
        (list (graphene:point3d-x p2)
              (graphene:point3d-y p2) (graphene:point3d-z p2))
        (list (graphene:point3d-x p3)
              (graphene:point3d-y p3) (graphene:point3d-z p3))))
=> ((0.0 0.0 0.0) (1.2 1.3 1.4) (1.2 1.3 1.4))
    @end{pre}
  @end{dictionary}
  @see-symbol{graphene:point3d-t}
  @see-macro{graphene:with-point3d}"
  (if vars
      (let ((var (if (listp (first vars)) (first vars) (list (first vars)))))
        `(with-point3d ,var
           (with-point3ds ,(rest vars)
             ,@body)))
      `(progn ,@body)))

(export 'with-point3ds)

;;; ----------------------------------------------------------------------------
;;; graphene_point3d_t
;;; ----------------------------------------------------------------------------

(cffi:defcstruct point3d-t
  (x :float)
  (y :float)
  (z :float))

#+liber-documentation
(setf (liber:alias-for-symbol 'point3d-t)
      "CStruct"
      (liber:symbol-documentation 'point3d-t)
 "@version{2023-12-3}
  @begin{short}
    The @symbol{graphene:point3d-t} structure is a data structure capable of
    describing a point with three coordinates.
  @end{short}
  @begin{pre}
(cffi:defcstruct point3d-t
  (x :float)
  (y :float)
  (z :float))
  @end{pre}
  Access the coordinates with the @fun{graphene:point3d-x},
  @fun{graphene:point3d-y} and @fun{graphene:point3d-z} functions. Use the
  @macro{graphene:with-point3d} and @macro{graphene:with-point3ds} macros to
  allocate a new @symbol{graphene:point3d-t} instance and initialize the point
  with values.
  @begin[Example]{dictionary}
    Allocate and initalize a point with values.
    @begin{pre}
(graphene:with-point3d (p 1.0 1/2 5)
  (list (graphene:point3d-x p)
        (graphene:point3d-y p)
        (graphene:point3d-z p)))
=> (1.0 0.5 5.0)
    @end{pre}
  @end{dictionary}
  @see-constructor{graphene:point3d-init}
  @see-constructor{graphene:point3d-init-from-point}
  @see-constructor{graphene:point3d-init-from-vec3}
  @see-slot{graphene:point3d-x}
  @see-slot{graphene:point3d-y}
  @see-slot{graphene:point3d-z}
  @see-symbol{graphene:point-t}")

(export 'point3d-t)

;;; --- Acessor Implementations ------------------------------------------------

;;; --- graphene:point3d-x -----------------------------------------------------

(defun (setf point3d-x) (value p)
  (setf (cffi:foreign-slot-value p '(:struct point3d-t) 'x)
        (coerce value 'single-float)))

(defun point3d-x (p)
  (cffi:foreign-slot-value p '(:struct point3d-t) 'x))

#+liber-documentation
(setf (liber:alias-for-function 'point3d-x)
      "Accessor"
      (documentation 'point3d-x 'function)
 "@version{2023-12-3}
  @syntax[]{(graphene:point3d-x p) => x}
  @syntax[]{(setf (graphene:point3d-x p) x)}
  @argument[p]{a @symbol{graphene:point3d-t} instance}
  @argument[x]{a float with the x coordinate}
  @begin{short}
    Accessor of the @code{x} slot of the @symbol{graphene:point3d-t} structure.
  @end{short}
  The @arg{x} value is coerced to a float before assignment.
  @begin[Examples]{dictionary}
    @begin{pre}
(graphene:with-point3d (p 0.5 1.0 1.5)
  (list (graphene:point3d-x p) (graphene:point3d-y p) (graphene:point3d-z p)))
=> (0.5 1.0 1.5)
(graphene:with-point3d (p)
  (setf (graphene:point3d-x p) 2.0
        (graphene:point3d-y p) 2.5
        (graphene:point3d-z p) 3.0)
  (list (graphene:point3d-x p) (graphene:point3d-y p) (graphene:point3d-z p)))
=> (2.0 2.5 3.0)
    @end{pre}
  @end{dictionary}
  @see-symbol{graphene:point3d-t}")

(export 'point3d-x)

;;; --- graphene:point3d-y -----------------------------------------------------

(defun (setf point3d-y) (value p)
  (setf (cffi:foreign-slot-value p '(:struct point3d-t) 'y)
        (coerce value 'single-float)))

(defun point3d-y (p)
  (cffi:foreign-slot-value p '(:struct point3d-t) 'y))

#+liber-documentation
(setf (liber:alias-for-function 'point3d-y)
      "Accessor"
      (documentation 'point3d-y 'function)
 "@version{2023-12-3}
  @syntax[]{(graphene:point3d-y p) => y}
  @syntax[]{(setf (graphene:point3d-y p) y)}
  @argument[point]{a @symbol{graphene:point3d-t} instance}
  @argument[x]{a float with the y coordinate}
  @begin{short}
    Accessor of the @code{y} slot of the @symbol{graphene:point3d-t} structure.
  @end{short}
  The @arg{y} value is coerced to a float before assignment. See the
  @fun{graphene:point3d-x} documentation for an example.
  @see-symbol{graphene:point3d-t}
  @see-function{graphene:point3d-x}")

(export 'point3d-y)

;;; --- graphene:point3d-z -----------------------------------------------------

(defun point3d-z (p)
  (cffi:foreign-slot-value p '(:struct point3d-t) 'z))

(defun (setf point3d-z) (value p)
  (setf (cffi:foreign-slot-value p '(:struct point3d-t) 'z)
        (coerce value 'single-float)))

#+liber-documentation
(setf (liber:alias-for-function 'point3d-z)
      "Accessor"
      (documentation 'point3d-z 'function)
 "@version{2023-12-3}
  @syntax[]{(graphene:point3d-z p) => z}
  @syntax[]{(setf (graphene:point3d-z p) z)}
  @argument[p]{a @symbol{graphene:point3d-t} instance}
  @argument[x]{a float with the z coordinate}
  @begin{short}
    Accessor of the @code{z} slot of the @symbol{graphene:point3d-t} structure.
  @end{short}
  The @arg{z} value is coerced to a float before assignment. See the
  @fun{graphene:point3d-x} documentation for an example.
  @see-symbol{graphene:point3d-t}
  @see-function{graphene:point3d-x}")

(export 'point3d-z)

;;; ----------------------------------------------------------------------------
;;; graphene_point3d_alloc ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_point3d_alloc" point3d-alloc)
    (:pointer (:struct point3d-t))
 #+liber-documentation
 "@version{2023-9-22}
  @return{The newly allocated @symbol{graphene:point3d-t} instance.}
  @begin{short}
    Allocates a new @symbol{graphene:point3d-t} instance.
  @end{short}
  The coordinates of the returned point are initialized to (0.0, 0.0, 0.0). Use
  the @fun{graphene:point3d-free} function to free the resources allocated by
  this function.
  @begin[Examples]{dictionary}
    It is possible to chain this function with the @fun{graphene:point3d-init}
    or @fun{graphene:point3d-init-from-point} functions.
    @begin{pre}
(defun point3d-new (x y z)
  (graphene:point3d-init (graphene:point3d-alloc) x y z))
(defun point3d-copy (p)
  (graphene:point3d-init-from-point (graphene:point3d-alloc) p))
    @end{pre}
  @end{dictionary}
  @see-symbol{graphene:point3d-t}
  @see-function{graphene:point-free}
  @see-function{graphene:point3d-init}
  @see-function{graphene:point3d-init-from-point}")

(export 'point3d-alloc)

;;; ----------------------------------------------------------------------------
;;; graphene_point3d_free ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_point3d_free" point3d-free) :void
 #+liber-documentation
 "@version{2023-12-3}
  @argument[p]{a @symbol{graphene:point3d-t} instance}
  @begin{short}
    Frees the resources allocated by the @fun{graphene:point3d-alloc} function.
  @end{short}
  @see-symbol{graphene:point3d-t}
  @see-function{graphene:point3d-alloc}"
  (p (:pointer (:struct point3d-t))))

(export 'point3d-free)

;;; ----------------------------------------------------------------------------
;;; graphene_point3d_zero ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_point3d_zero" point3d-zero)
    (:pointer (:struct point3d-t))
 #+liber-documentation
 "@version{2023-9-22}
  @return{The @symbol{graphene:point3d-t} instance with a zero point.}
  @begin{short}
    Returns a point with all three coordinates set to zero.
  @end{short}
  @begin[Examples]{dictionary}
    @begin{pre}
(values (graphene:point3d-x (graphene:point3d-zero))
        (graphene:point3d-y (graphene:point3d-zero))
        (graphene:point3d-z (graphene:point3d-zero)))
=> 0.0
=> 0.0
=> 0.0
    @end{pre}
  @end{dictionary}
  @see-symbol{graphene:point3d-t}")

(export 'point3d-zero)

;;; ----------------------------------------------------------------------------
;;; graphene_point3d_init ()
;;; ----------------------------------------------------------------------------

(defun point3d-init (p x y z)
 #+liber-documentation
 "@version{2023-12-3}
  @argument[p]{a @symbol{graphene:point3d-t} instance}
  @argument[x]{a number coerced to a float with the x coordinate}
  @argument[y]{a number coerced to a float with the y coordinate}
  @argument[z]{a number coerced to a float with the z coordinate}
  @return{The initialized @symbol{graphene:point3d-t} instance.}
  @begin{short}
    Initializes the point to the given @arg{x}, @arg{y}, and @arg{z}
    coordinates.
  @end{short}
  It is safe to call this function multiple times.
  @see-symbol{graphene:point3d-t}"
  (cffi:foreign-funcall "graphene_point3d_init"
                        (:pointer (:struct point3d-t)) p
                        :float (coerce x 'single-float)
                        :float (coerce y 'single-float)
                        :float (coerce z 'single-float)
                        (:pointer (:struct point3d-t))))

(export 'point3d-init)

;;; ----------------------------------------------------------------------------
;;; graphene_point3d_init_from_point ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_point3d_init_from_point" point3d-init-from-point)
    (:pointer (:struct point3d-t))
 #+liber-documentation
 "@version{2023-12-3}
  @argument[p]{a @symbol{graphene:point3d-t} instance}
  @argument[src]{a @symbol{graphene:point3d-t} instance to use}
  @return{The initialized @symbol{graphene:point3d-t} instance.}
  @short{Initializes the point using the coordinates of @arg{src}.}
  @see-symbol{graphene:point3d-t}"
  (p (:pointer (:struct point3d-t)))
  (src (:pointer (:struct point3d-t))))

(export 'point3d-init-from-point)

;;; ----------------------------------------------------------------------------
;;; graphene_point3d_init_from_vec3 ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_point3d_init_from_vec3" point3d-init-from-vec3)
   (:pointer (:struct point3d-t))
 #+liber-documentation
 "@version{2023-12-3}
  @argument[p]{a @symbol{graphene:point3d-t} instance}
  @argument[v]{a @symbol{graphene:vec3-t} instance to use}
  @return{The initialized @symbol{graphene:point3d-t} instance.}
  @short{Initializes the point using the components of the given vector.}
  @see-symbol{graphene:point3d-t}
  @see-symbol{graphene:vec3-t}"
  (p (:pointer (:struct point3d-t)))
  (v :pointer)) ; vec3-t not known at this point

(export 'point3d-init-from-vec3)

;;; ----------------------------------------------------------------------------
;;; graphene_point3d_to_vec3 ()
;;; ----------------------------------------------------------------------------

(defun point3d-to-vec3 (p v)
 #+liber-documentation
 "@version{2023-12-3}
  @argument[p]{a @symbol{graphene:point3d-t} instance}
  @argument[v]{a @symbol{graphene:vec3-t} instance}
  @return{The @symbol{graphene:vec3-t} instance with the coordinates of the
    point.}
  @short{Stores the coordinates of the given point into a vector.}
  @begin[Examples]{dictionary}
    @begin{pre}
(graphene:with-point3d (p 1.0 2.0 3.0)
  (graphene:with-vec3 (v)
    (graphene:vec3-to-float (graphene:point3d-to-vec3 p v))))
=> (1.0 2.0 3.0)
    @end{pre}
  @end{dictionary}
  @see-symbol{graphene:point3d-t}
  @see-symbol{graphene:vec3-t}"
  (cffi:foreign-funcall "graphene_point3d_to_vec3"
                        (:pointer (:struct point3d-t)) p
                        :pointer v ; vec3-t not known at this point
                        :void)
  v)

(export 'point3d-to-vec3)

;;; ----------------------------------------------------------------------------
;;; graphene_point3d_equal ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_point3d_equal" point3d-equal) :bool
 #+liber-documentation
 "@version{2023-9-22}
  @argument[a]{a @symbol{graphene:point3d-t} instance}
  @argument[b]{a @symbol{graphene:point3d-t} instance}
  @return{@em{True}, if the points have the same coordinates, otherwise
    @em{false}.}
  @begin{short}
    Checks whether two given points are equal.
  @end{short}
  This function accounts for floating point fluctuations. If you want to
  control the fuzziness of the match, you can use the
  @fun{graphene:point3d-near} function instead.
  @see-symbol{graphene:point3d-t}
  @see-function{graphene:point3d-near}"
  (a (:pointer (:struct point3d-t)))
  (b (:pointer (:struct point3d-t))))

(export 'point3d-equal)

;;; ----------------------------------------------------------------------------
;;; graphene_point3d_near ()
;;; ----------------------------------------------------------------------------

(defun point3d-near (a b epsilon)
 #+liber-documentation
 "@version{2023-9-22}
  @argument[a]{a @symbol{graphene:point3d-t} instance}
  @argument[b]{a @symbol{graphene:point3d-t} instance}
  @argument[epsilon]{a number coerced to a float with the threshold between the
    two points}
  @return{@em{True}, if the distance between the points is within
    @arg{epsilon}, otherwise @em{false}.}
  @begin{short}
    Checks whether the two points @arg{a} and @arg{b} are within the threshold
    of @arg{epsilon} equal.
  @end{short}
  @see-symbol{graphene:point-t}
  @see-function{graphene:point3d-equal}"
  (cffi:foreign-funcall "graphene_point3d_near"
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
 "@version{2023-12-3}
  @syntax[]{(graphene:point-distance a b) => dist, delta}
  @argument[a]{a @symbol{graphene:point3d-t} instance}
  @argument[b]{a @symbol{graphene:point3d-t} instance}
  @argument[delta]{a @symbol{graphene:vec3-t} instance with the calculated
    distance components on the x, y, and z axis}
  @argument[dist]{a float with the distance between two points}
  @short{Computes the distance between the two given points.}
  @begin[Example]{dictionary}
    @begin{pre}
(graphene:with-vec3 (v)
  (graphene:with-point3ds ((p1 0 0 0) (p2 1 1 1))
    (multiple-value-bind (dist delta)
        (graphene:point3d-distance p1 p2 v)
      (values dist
              (graphene:vec3-to-float delta)))))
=> 1.7320508
=> (1.0 1.0 1.0)
    @end{pre}
  @end{dictionary}
  @see-symbol{graphene:point3d-t}
  @see-symbol{graphene:vec3-t}"
  (values (cffi:foreign-funcall "graphene_point3d_distance"
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
 "@version{2023-9-22}
  @argument[a]{a @symbol{graphene:point3d-t} instance}
  @argument[b]{a @symbol{graphene:point3d-t} instance}
  @argument[factor]{a number coerced to a double float with the linear
    interpolation factor}
  @argument[result]{a @symbol{graphene:point3d-t} instance for the interpolated
    point}
  @return{The @symbol{graphene:point3d-t} instance with the interpolated point.}
  @begin{short}
    Linearly interpolates the coordinates of @arg{a} and @arg{b} using the
    given @arg{factor}.
  @end{short}
  @begin[Examples]{dictionary}
    @begin{pre}
(graphene:with-point3ds ((a 0 0 0) (b 1 2 3) result)
  (graphene:point3d-interpolate a b 0.5 result)
  (values (graphene:point3d-x result)
          (graphene:point3d-y result)
          (graphene:point3d-z result)))
=> 0.5
=> 1.0
=> 1.5
    @end{pre}
  @end{dictionary}
  @see-symbol{graphene:point3d-t}"
  (cffi:foreign-funcall "graphene_point3d_interpolate"
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

(defun point3d-scale (p factor result)
 #+liber-documentation
 "@version{2023-12-3}
  @argument[p]{a @symbol{graphene:point3d-t} instance}
  @argument[factor]{a number coerced to a float with the scaling factor}
  @argument[result]{a @symbol{graphene:point3d-t} instance for the scaled point}
  @return{The @symbol{graphene:point3d-t} instance with the scaled point.}
  @short{Scales the coordinates of the given point by the given factor.}
  @see-symbol{graphene:point3d-t}"
  (cffi:foreign-funcall "graphene_point3d_scale"
                        (:pointer (:struct point3d-t)) p
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
 "@version{2023-9-22}
  @argument[a]{a @symbol{graphene:point3d-t} instance}
  @argument[b]{a @symbol{graphene:point3d-t} instance}
  @argument[result]{a @symbol{graphene:point3d-t} instance for the cross
    product}
  @return{The @symbol{graphene:point3d-t} instance with the cross product.}
  @short{Computes the cross product of the two given points.}
  @see-symbol{graphene:point3d-t}"
  (cffi:foreign-funcall "graphene_point3d_cross"
                        (:pointer (:struct point3d-t)) a
                        (:pointer (:struct point3d-t)) b
                        (:pointer (:struct point3d-t)) result
                        :void)
  result)

(export 'point3d-cross)

;;; ----------------------------------------------------------------------------
;;; graphene_point3d_dot ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_point3d_dot" point3d-dot) :float
 #+liber-documentation
 "@version{2023-9-22}
  @argument[a]{a @symbol{graphene:point3d-t} instance}
  @argument[b]{a @symbol{graphene:point3d-t} instance}
  @return{The float with the value of the dot product.}
  @short{Computes the dot product of the two given points.}
  @see-symbol{graphene:point3d-t}"
  (a (:pointer (:struct point3d-t)))
  (b (:pointer (:struct point3d-t))))

(export 'point3d-dot)

;;; ----------------------------------------------------------------------------
;;; graphene_point3d_length ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_point3d_length" point3d-length) :float
 #+liber-documentation
 "@version{2023-9-22}
  @argument[p]{a @symbol{graphene:point3d-t} instance}
  @return{The float with the value of the length of the vector represented by
    the point.}
  @begin{short}
    Computes the length of the vector represented by the coordinates of the
    given point.
  @end{short}
  @see-symbol{graphene:point3d-t}"
  (p (:pointer (:struct point3d-t))))

(export 'point3d-length)

;;; ----------------------------------------------------------------------------
;;; graphene_point3d_normalize ()
;;; ----------------------------------------------------------------------------

(defun point3d-normalize (p result)
 #+liber-documentation
 "@version{2023-12-3}
  @argument[point]{a @symbol{graphene:point3d-t} instance}
  @argument[result]{a @symbol{graphene:point3d-t} instance}
  @return{The @symbol{graphene:point3d-t} instance with the normalized point.}
  @begin{short}
    Computes the normalization of the vector represented by the coordinates of
    the given point.
  @end{short}
  @see-symbol{graphene:point3d-t}"
  (cffi:foreign-funcall "graphene_point3d_normalize"
                        (:pointer (:struct point3d-t)) p
                        (:pointer (:struct point3d-t)) result
                        :void)
  result)

(export 'point3d-normalize)

;;; ----------------------------------------------------------------------------
;;; graphene_point3d_normalize_viewport ()
;;; ----------------------------------------------------------------------------

(defun point3d-normalize-viewport (p viewport znear zfar result)
 #+liber-documentation
 "@version{2023-12-3}
  @argument[p]{a @symbol{graphene:point3d-t} instance}
  @argument[viewport]{a @symbol{graphene:rect-t} instance representing a
    viewport}
  @argument[znear]{a number coerced to a float with the coordinate of the near
    clipping plane, of 0.0 for the default near clipping plane}
  @argument[zfar]{a number coerced to a float with the coordinate of the far
    clipping plane, of 1.0 for the default far clipping plane}
  @argument[result]{a @symbol{graphene:point3d-t} instance for the nomalized
    point}
  @return{The @symbol{graphene:point3d-t} instance with the normalized point.}
  @begin{short}
    Normalizes the coordinates of the point using the given viewport and
    clipping planes.
  @end{short}
  The coordinates of the resulting point will be in the [-1.0, 1.0] range.
  @see-symbol{graphene:point3d-t}
  @see-symbol{graphene:rect-t}"
  (cffi:foreign-funcall "graphene_point3d_normalize_viewport"
                        (:pointer (:struct point3d-t)) p
                        (:pointer (:struct rect-t)) viewport
                        :float (coerce znear 'single-float)
                        :float (coerce zfar 'single-float)
                        (:pointer (:struct point3d-t)) result
                        :void)
  result)

(export 'point3d-normalize-viewport)

;;; --- End of file graphene-point3d.lisp --------------------------------------

