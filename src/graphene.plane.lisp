;;; ----------------------------------------------------------------------------
;;; graphene.plane.lisp
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
;;; Plane
;;;
;;;     A plane in 3D space
;;;
;;; Types and Values
;;;
;;;     graphene_plane_t
;;;
;;; Functions
;;;
;;;     graphene_plane_alloc
;;;     graphene_plane_free
;;;     graphene_plane_init
;;;     graphene_plane_init_from_vec4
;;;     graphene_plane_init_from_plane
;;;     graphene_plane_init_from_point
;;;     graphene_plane_init_from_points
;;;     graphene_plane_normalize
;;;     graphene_plane_negate
;;;     graphene_plane_equal
;;;     graphene_plane_distance
;;;     graphene_plane_transform
;;;     graphene_plane_get_normal
;;;     graphene_plane_get_constant
;;; ----------------------------------------------------------------------------

(in-package :graphene)

(defmacro with-plane ((var &rest args) &body body)
 #+liber-documentation
 "@version{2024-1-20}
  @syntax{(graphene:with-plane (plane) body) => result}
  @syntax{(graphene:with-plane (plane plane1) body) => result}
  @syntax{(graphene:with-plane (plane (v graphene:vec4-t) body) => result}
  @syntax{(graphene:with-plane (plane normal constant) body) => result}
  @syntax{(graphene:with-plane (plane normal (point graphene:point3d-t)) body)
    => result}
  @syntax{(graphene:with-plane (plane a b c) body) => result}
  @argument[plane]{a @symbol{graphene:plane-t} instance to create and
    initialize}
  @argument[plane1]{a @symbol{graphene:plane-t} instance to use for
    initialization}
  @argument[v]{a @symbol{graphene:vec4-t} instance to use for initialization}
  @argument[normal]{a @symbol{graphene:vec3-t} instance to use for
    initialization}
  @argument[point]{a @symbol{graphene:point3d-t} instance to use for
    initialization}
  @argument[a, b, c]{a @symbol{graphene:point3d-t} instance}
  @argument[constant]{a float value}
  @begin{short}
    The @fun{graphene:with-plane} macro allocates a new
    @symbol{graphene:plane-t} instance, initializes the box with the given
    values and executes the body that uses the box.
  @end{short}
  After execution of the body the allocated memory for the plane is released.
  @begin[Note]{dictionary}
    The memory is allocated with the @fun{graphene:plane-alloc} function and
    released with the @fun{graphene:plane-free} function.
  @end{dictionary}
  @see-symbol{graphene:plane-t}
  @see-symbol{graphene:vec4-t}
  @see-symbol{graphene:vec3-t}
  @see-symbol{graphene:point3d-t}
  @see-macro{graphene:with-planes}
  @see-function{graphene:plane-alloc}
  @see-function{graphene:plane-free}"
  (cond ((null args)
         ;; No arguments, the default is initialization with zeros.
         `(let ((,var (plane-alloc)))
            (plane-init-from-vec4 ,var (vec4-zero))
            (unwind-protect
              (progn ,@body)
              (plane-free ,var))))
        ((null (second args))
         ;; One argument
         (destructuring-bind (arg &optional type1) (mklist (first args))
           (cond ((or (not type1)
                      (eq type1 'plane-t))
                  ;; One argument with no type or of type plane-t
                  `(let ((,var (plane-alloc)))
                     (plane-init-from-plane ,var ,arg)
                     (unwind-protect
                       (progn ,@body)
                       (plane-free ,var))))
                 ((eq type1 'vec4-t)
                  ;; One argument with type vec4-t
                  `(let ((,var (plane-alloc)))
                     (plane-init-from-vec4 ,var ,arg)
                     (unwind-protect
                       (progn ,@body)
                       (plane-free ,var))))
                 (t
                  (error "Syntax error in GRAPHENE:WITH-PLANE")))))
        ((null (third args))
         ;; Two arguments
         (destructuring-bind ((arg1 &optional type1)
                              (arg2 &optional type2))
             (list (mklist (first args)) (mklist (second args)))
           (cond ((and (or (not type1)
                           (eq type1 'vec3-t))
                       (or (not type2)
                           (eq type2 :float)))
                  ;; First argument with no type or of type vec3-t and
                  ;; second argument with no type or type point3d-t
                  `(let ((,var (plane-alloc)))
                     (plane-init ,var ,arg1 ,arg2)
                     (unwind-protect
                       (progn ,@body)
                       (plane-free ,var))))
                 ((and (or (not type1)
                           (eq type1 'vec3-t))
                       (eq type2 'point3d-t))
                  ;; First argument with no type or of type vec3-t and
                  ;; second argument with type point3d-t
                  `(let ((,var (plane-alloc)))
                     (plane-init-from-point ,var ,arg1 ,arg2)
                     (unwind-protect
                       (progn ,@body)
                       (plane-free ,var))))
                 (t
                  (error "Syntax error in GRAPHENE:WITH-PLANE")))))
        ((null (fourth args))
         ;; Three arguments
         `(let ((,var (plane-alloc)))
            (plane-init-from-points ,var ,@args)
            (unwind-protect
            (progn ,@body)
            (plane-free ,var))))
        (t
         (error "Syntax error in GRAPHENE:WITH-PLANE"))))

(export 'with-plane)

(defmacro with-planes (vars &body body)
 #+liber-documentation
 "@version{2024-1-20}
  @syntax{(graphene:with-planes (plane1 ... planen) body) => result}
  @argument[plane1 ... planen]{the newly created @symbol{graphene:plane-t}
    instances}
  @argument[body]{a body that uses the bindings @arg{plane1 ... planen}}
  @begin{short}
    The @fun{graphene:with-planes} macro creates new variable bindings and
    executes the body that use these bindings.
  @end{short}
  The macro performs the bindings sequentially, like the @sym{let*} macro.

  Each plane can be initialized with values using the syntax for the
  @fun{graphene:with-plane} macro. See also the @fun{graphene:with-plane}
  documentation.
  @see-symbol{graphene:plane-t}
  @see-macro{graphene:with-plane}"
  (if vars
      (let ((var (mklist (first vars))))
        `(with-plane ,var
           (with-planes ,(rest vars)
             ,@body)))
      `(progn ,@body)))

(export 'with-planes)

;;; ----------------------------------------------------------------------------
;;; graphene_plane_t
;;; ----------------------------------------------------------------------------

;; We need the size of the graphene:plant-t structure for the implementation of
;; the graphene:frustum-planes function. This function needs a size of 8 float
;; values and not 5 float values. Is this correct through the whole library?
(cffi:defcstruct plane-t
  (normal :float :count 4)
  (constant :float :count 4))

#+liber-documentation
(setf (liber:alias-for-symbol 'plane-t)
      "CStruct"
      (liber:symbol-documentation 'plane-t)
 "@version{2023-12-7}
  @begin{short}
    The @symbol{graphene:plane-t} structure is a structure representing a plane
    that extends infinitely in 3D space.
  @end{short}
  The plane is described using the Hessian normal form of a unit length normal
  vector pointing towards the origin, and a constant distance from the origin
  along the normal vector.")

(export 'plane-t)

;;; ----------------------------------------------------------------------------
;;; graphene_plane_alloc ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_plane_alloc" plane-alloc) (:pointer (:struct plane-t))
 #+liber-documentation
 "@version{2023-12-7}
  @return{The newly allocated @symbol{graphene:plane-t} instance.}
  @begin{short}
    Allocates a new @symbol{graphene:plane-t} instance.
  @end{short}
  The contents of the returned instance are undefined. Use the
  @fun{graphene:plane-free} function to free the resources allocated by this
  function.
  @see-symbol{graphene:plane-t}
  @see-function{graphene:plane-free}")

(export 'plane-alloc)

;;; ----------------------------------------------------------------------------
;;; graphene_plane_free ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_plane_free" plane-free) :void
 #+liber-documentation
 "@version{2023-12-7}
  @argument[plane]{a @symbol{graphene:plane-t} instance}
  @begin{short}
    Frees the resources allocated by the @fun{graphene:plane-alloc} function.
  @end{short}
  @see-symbol{graphene:plane-t}
  @see-function{graphene:plane-alloc}"
  (plane (:pointer (:struct plane-t))))

(export 'plane-free)

;;; ----------------------------------------------------------------------------
;;; graphene_plane_init ()
;;; ----------------------------------------------------------------------------

(defun plane-init (plane normal constant)
 #+liber-documentation
 "@version{2023-12-7}
  @argument[plane]{a @symbol{graphene:plane-t} instance to initialize}
  @argument[normal]{a @symbol{graphene:vec3-t} instance with a unit length
    normal vector defining the plane pointing towards the origin, if unset, the
    x axis is used by default}
  @argument[constant]{a number coerced to a float with the distance from the
    origin to the plane along the normal vector, the sign determines the
    half-space occupied by the plane}
  @return{The initialized @symbol{graphene:plane-t} instance.}
  @begin{short}
    Initializes the given plane using the given normal vector and constant
    value.
  @end{short}
  @see-symbol{graphene:plane-t}
  @see-symbol{graphene:vec3-t}"
  (let ((normal (if normal normal (vec3-x-axis))))
    (cffi:foreign-funcall "graphene_plane_init"
                          (:pointer (:struct plane-t)) plane
                          (:pointer (:struct vec3-t)) normal
                          :float (coerce constant 'single-float)
                          (:pointer (:struct plane-t)))))

(export 'plane-init)

;;; ----------------------------------------------------------------------------
;;; graphene_plane_init_from_vec4 ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_plane_init_from_vec4" plane-init-from-vec4)
    (:pointer (:struct plane-t))
 #+liber-documentation
 "@version{2023-12-7}
  @argument[plane]{a @symbol{graphene:plane-t} instance to initialize}
  @argument[vector]{a @symbol{graphene:vec4-t} instance with the vector
    containing the normal vector in its first three components, and the
    distance in its fourth component}
  @return{The initialized @symbol{graphene:plane-t} instance.}
  @begin{short}
    Initializes the given plane using the components of the given vector.
  @end{short}
  @see-symbol{graphene:plane-t}
  @see-symbol{graphene:vec4-t}"
  (plane (:pointer (:struct plane-t)))
  (vector (:pointer (:struct vec4-t))))

(export 'plane-init-from-vec4)

;;; ----------------------------------------------------------------------------
;;; graphene_plane_init_from_plane ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_plane_init_from_plane" plane-init-from-plane)
    (:pointer (:struct plane-t))
 #+liber-documentation
 "@version{#2023-12-7}
  @argument[plane]{a @symbol{graphene:plane-t} instance to initialize}
  @argument[source]{a @symbol{graphene:plane-t} instance}
  @return{The initialized @symbol{graphene:plane-t} instance.}
  @begin{short}
    Initializes the given plane using the normal vector and constant of
    another plane.
  @end{short}
  @see-symbol{graphene:plane-t}"
  (plane (:pointer (:struct plane-t)))
  (source (:pointer (:struct plane-t))))

(export 'plane-init-from-plane)

;;; ----------------------------------------------------------------------------
;;; graphene_plane_init_from_point ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_plane_init_from_point" plane-init-from-point)
    (:pointer (:struct plane-t))
 #+liber-documentation
 "@version{#2023-12-7}
  @argument[plane]{a @symbol{graphene:plane-t} instance to initialize}
  @argument[normal]{a @symbol{graphene:vec3-t} instance with the normal vector
    defining the plane pointing towards the origin}
  @argument[point]{a @symbol{point3d-t} instance}
  @return{The initialized @symbol{graphene:plane-t} instance.}
  @begin{short}
    Initializes the given plane using the given normal vector and an arbitrary
    co-planar point.
  @end{short}
  @see-symbol{graphene:plane-t}
  @see-symbol{graphene:vec3-t}
  @see-symbol{graphene:point3d-t}"
  (plane (:pointer (:struct plane-t)))
  (normal (:pointer (:struct vec3-t)))
  (point (:pointer (:struct point3d-t))))

(export 'plane-init-from-point)

;;; ----------------------------------------------------------------------------
;;; graphene_plane_init_from_points ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_plane_init_from_points" plane-init-from-points)
    (:pointer (:struct plane-t))
 #+liber-documentation
 "@version{#2023-12-7}
  @argument[plane]{a @symbol{graphene:plane-t} instance to initialize}
  @argument[a]{a @symbol{graphene:point3d-t} instance}
  @argument[b]{a @symbol{graphene:point3d-t} instance}
  @argument[c]{a @symbol{graphene:point3d-t} instance}
  @return{The initialized @symbol{graphene:plane-t} instance.}
  @begin{short}
    Initializes the given plane using the 3 provided co-planar points.
  @end{short}
  The winding order is counter-clockwise, and determines which direction the
  normal vector will point.
  @see-symbol{graphene:plane-t}
  @see-symbol{graphene:point3d-t}"
  (plane (:pointer (:struct plane-t)))
  (a (:pointer (:struct point3d-t)))
  (b (:pointer (:struct point3d-t)))
  (c (:pointer (:struct point3d-t))))

(export 'plane-init-from-points)

;;; ----------------------------------------------------------------------------
;;; graphene_plane_normalize ()
;;; ----------------------------------------------------------------------------

(defun plane-normalize (plane result)
 #+liber-documentation
 "@version{#2023-12-7}
  @argument[plane]{a @symbol{graphene:plane-t} instance to initialize}
  @argument[result]{a @symbol{graphene:plane-t} instance for the result}
  @return{The normalized @symbol{graphene:plane-t} instance.}
  @begin{short}
    Normalizes the vector of the given plane, and adjusts the constant
    accordingly.
  @end{short}
  @see-symbol{graphene:plane-t}"
  (cffi:foreign-funcall "graphene_plane_normalize"
                        (:pointer (:struct plane-t)) plane
                        (:pointer (:struct plane-t)) result
                        :void)
  result)

(export 'plane-normalize)

;;; ----------------------------------------------------------------------------
;;; graphene_plane_negate ()
;;; ----------------------------------------------------------------------------

(defun plane-negate (plane result)
 #+liber-documentation
 "@version{#2023-12-7}
  @argument[plane]{a @symbol{graphene:plane-t} instance to initialize}
  @argument[result]{a @symbol{graphene:plane-t} instance for the result}
  @return{The normalized @symbol{graphene:plane-t} instance.}
  @begin{short}
    Negates the normal vector and constant of the plane, effectively mirroring
    the plane across the origin.
  @end{short}
  @see-symbol{graphene:plane-t}"
  (cffi:foreign-funcall "graphene_plane_negate"
                        (:pointer (:struct plane-t)) plane
                        (:pointer (:struct plane-t)) result
                        :void)
  result)

(export 'plane-negate)

;;; ----------------------------------------------------------------------------
;;; graphene_plane_equal ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_plane_equal" plane-equal) :bool
 #+liber-documentation
 "@version{#2023-12-7}
  @argument[a]{a @symbol{graphene:plane-t} instance}
  @argument[b]{a @symbol{graphene:plane-t} instance}
  @return{@em{True} if the given planes are equal.}
  @short{Checks whether the two given planes are equal.}
  @see-symbol{graphene:plane-t}"
  (a (:pointer (:struct plane-t)))
  (b (:pointer (:struct plane-t))))

(export 'plane-equal)

;;; ----------------------------------------------------------------------------
;;; graphene_plane_distance ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_plane_distance" plane-distance) :float
 #+liber-documentation
 "@version{#2023-12-7}
  @argument[plane]{a @symbol{graphene:plane-t} instance}
  @argument[point]{a @symbol{graphene:point3d-t} instance}
  @return{The float with the distance of the given point from the plane.}
  @short{Computes the distance of the point from the plane.}
  @see-symbol{graphene:plane-t}
  @see-symbol{graphene:point3d-t}"
  (plane (:pointer (:struct plane-t)))
  (point (:pointer (:struct point3d-t))))

(export 'plane-distance)

;;; ----------------------------------------------------------------------------
;;; graphene_plane_transform
;;; ----------------------------------------------------------------------------

(defun plane-transform (plane matrix normal result)
 #+liber-documentation
 "@version{#2024-12-29}
  @argument[plane]{a @symbol{graphene:plane-t} instance}
  @argument[matrix]{a @symbol{graphene:matrix-t} instance}
  @argument[normal]{a @symbol{graphene:matrix-t} instance}
  @argument[result]{a @symbol{graphene:plane-t} instance}
  @return{The transformed @symbol{graphene:plane-t} instance.}
  @begin{short}
    Transforms a plane using the given matrix and normal matrix.
  @end{short}
  If the normal matrix is @code{nil}, a transformation matrix for the plane
  normal will be computed from @arg{matrix}. If you are transforming multiple
  planes using the same matrix it is recommended to compute the normal matrix
  beforehand to avoid incurring in the cost of recomputing it every time.
  @see-symbol{graphene:plane-t}
  @see-symbol{graphene:matrix-t}"
  (let ((normal (or normal (cffi:null-pointer))))
    (cffi:foreign-funcall "graphene_plane_transform"
                          (:pointer (:struct plane-t)) plane
                          (:pointer (:struct matrix-t)) matrix
                          (:pointer (:struct matrix-t)) normal
                          (:pointer (:struct plane-t)) result
                          :void)
    result))

(export 'plane-transform)

;;; ----------------------------------------------------------------------------
;;; graphene_plane_get_normal ()
;;; ----------------------------------------------------------------------------

(defun plane-normal (plane normal)
 #+liber-documentation
 "@version{2023-12-7}
  @argument[plane]{a @symbol{graphene:plane-t} instance}
  @argument[normal]{a @symbol{graphene:vec3-t} instance for the normal vector}
  @return{The @symbol{graphene:vec3-t} instance with the normal vector.}
  @begin{short}
    Retrieves the normal vector pointing towards the origin of the given plane.
  @end{short}
  @see-symbol{graphene:plane-t}
  @see-symbol{graphene:vec3-t}"
  (cffi:foreign-funcall "graphene_plane_get_normal"
                        (:pointer (:struct plane-t)) plane
                        (:pointer (:struct vec3-t)) normal
                        :void)
  normal)

(export 'plane-normal)

;;; ----------------------------------------------------------------------------
;;; graphene_plane_get_constant ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_plane_get_constant" plane-constant) :float
 #+liber-documentation
 "@version{2023-12-7}
  @argument[plane]{a @symbol{graphene:plane-t} instance}
  @return{The float with the constant value of the plane.}
  @begin{short}
    Retrieves the distance along the normal vector of the given plane from the
    origin.
  @end{short}
  @see-symbol{graphene:plane-t}"
  (plane (:pointer (:struct plane-t))))

(export 'plane-constant)

;;; --- End of file graphene.plane.lisp ----------------------------------------
