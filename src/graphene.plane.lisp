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

(defmacro with-graphene-plane ((var &rest args) &body body)
  (cond ((not args)
         ;; No arguments, the default is initialization with zeros.
         `(let ((,var (plane-alloc)))
            (plane-init-from-vec4 ,var (vec4-zero))
            (unwind-protect
              (progn ,@body)
              (plane-free ,var))))
        ((not (second args))
         ;; One argument
         (destructuring-bind (arg &optional type)
             (if (listp (first args)) (first args) (list (first args)))
           (cond ((or (not type)
                      (eq type 'plane-t))
                  ;; One argument with no type or of type plane-t
                  `(let ((,var (plane-alloc)))
                     (plane-init-from-plane ,var ,arg)
                     (unwind-protect
                       (progn ,@body)
                       (plane-free ,var))))
                 ((eq type 'vec4-t)
                  ;; One argument with type vec4-t
                  `(let ((,var (plane-alloc)))
                     (plane-init-from-vec4 ,var ,arg)
                     (unwind-protect
                       (progn ,@body)
                       (plane-free ,var))))
                 (t
                  (error "Type error in WITH-GRAPHENE-PLANE")))))
        ((not (third args))
         ;; Two arguments
         (destructuring-bind (arg1 &optional type1)
             (if (listp (first args)) (first args) (list (first args)))
           (destructuring-bind (arg2 &optional type2)
               (if (listp (second args)) (second args) (list (second args)))
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
                    (error "Type error in WITH-GRAPHENE-PLANE"))))))
        ((not (fourth args))
         ;; Three arguments
         `(let ((,var (plane-alloc)))
            (plane-init-from-points ,var ,@args)
            (unwind-protect
            (progn ,@body)
            (plane-free ,var))))
        (t
         (error "Syntax error in WITH-GRAPHENE-PLANE"))))

(export 'with-graphene-plane)

(defmacro with-graphene-planes (vars &body body)
  (if vars
      (let ((var (if (listp (first vars)) (first vars) (list (first vars)))))
        `(with-graphene-plane ,var
           (with-graphene-planes ,(rest vars)
             ,@body)))
      `(progn ,@body)))

(export 'with-graphene-planes)

;;; ----------------------------------------------------------------------------
;;; graphene_plane_t
;;;
;;; typedef struct {
;;; } graphene_plane_t;
;;;
;;; A 2D plane that extends infinitely in a 3D volume.
;;;
;;; The contents of the graphene_plane_t are private, and should not be
;;; modified directly.
;;;
;;; Since 1.2
;;; ----------------------------------------------------------------------------

(cffi:defcstruct plane-t)

#+liber-documentation
(setf (liber:alias-for-symbol 'plane-t)
      "CStruct"
      (liber:symbol-documentation 'plane-t)
 "@version{#2022-9-30}
  @begin{short}
    The @sym{plane-t} structure is a structure representing a plane that extends
    infinitely in 3D space, described using the Hessian normal form of a unit
    length normal vector pointing towards the origin, and a constant distance
    from the origin along the normal vector.
  @end{short}")

(export 'plane-t)

;;; ----------------------------------------------------------------------------
;;; graphene_plane_alloc ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_plane_alloc" plane-alloc) (:pointer (:struct plane-t))
 #+liber-documentation
 "@version{#2022-9-25}
  @return{The newly allocated @symbol{plane-t} instance. Use the
    @fun{plane-free} function to free the resources allocated by this function.}
  @begin{short}
    Allocates a new @symbol{plane-t} instance.
  @end{short}
  The contents of the returned instance are undefined.
  @see-symbol{plane-t}
  @see-function{plane-free}")

(export 'plane-alloc)

;;; ----------------------------------------------------------------------------
;;; graphene_plane_free ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_plane_free" plane-free) :void
 #+liber-documentation
 "@version{#2022-9-25}
  @argument[plane]{a @symbol{plane-t} instance}
  @begin{short}
    Frees the resources allocated by the @fun{plane-alloc} function.
  @end{short}
  @see-symbol{plane-t}
  @see-function{plane-alloc}"
  (plane (:pointer (:struct plane-t))))

(export 'plane-free)

;;; ----------------------------------------------------------------------------
;;; graphene_plane_init ()
;;; ----------------------------------------------------------------------------

(defun plane-init (plane normal constant)
 #+liber-documentation
 "@version{#2022-9-30}
  @argument[plane]{a @symbol{plane-t} instance to initialize}
  @argument[normal]{a @symbol{vec3-t} instance with a unit length normal vector
    defining the plane pointing towards the origin, if unset, we use the x axis
    by default}
  @argument[constant]{a single float with the distance from the origin to the
    plane along the normal vector, the sign determines the half-space occupied
    by the plane}
  @return{A initialized @symbol{plane-t} instance.}
  @begin{short}
    Initializes the given plane using the given normal vector and constant
    value.
  @end{short}
  @see-symbol{plane-t}
  @see-symbol{vec3-t}"
  (cffi:foreign-funcall "graphene_plane_init"
                        (:pointer (:struct plane-t)) plane
                        (:pointer (:struct vec3-t)) normal
                        :float (coerce constant 'single-float)
                        (:pointer (:struct plane-t))))

(export 'plane-init)

;;; ----------------------------------------------------------------------------
;;; graphene_plane_init_from_vec4 ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_plane_init_from_vec4" plane-init-from-vec4)
    (:pointer (:struct plane-t))
 #+liber-documentation
 "@version{#2022-9-30}
  @argument[plane]{a @symbol{plane-t} instance to initialize}
  @argument[vector]{a @symbol{vec4-t} instance with the vector containing the
    normal vector in its first three components, and the distance in its fourth
    component}
  @return{A initialized @symbol{plane-t} instance.}
  @begin{short}
    Initializes the given plane using the components of the given vector.
  @end{short}
  @see-symbol{plane-t}
  @see-symbol{vec3-t}"
  (plane (:pointer (:struct plane-t)))
  (vector (:pointer (:struct vec4-t))))

(export 'plane-init-from-vec4)

;;; ----------------------------------------------------------------------------
;;; graphene_plane_init_from_plane ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_plane_init_from_plane" plane-init-from-plane)
    (:pointer (:struct plane-t))
 #+liber-documentation
 "@version{#2022-9-30}
  @argument[plane]{a @symbol{plane-t} instance to initialize}
  @argument[source]{a @symbol{plane-t} instance}
  @return{A initialized @symbol{plane-t} instance.}
  @begin{short}
    Initializes the given plane using the normal vector and constant of
    another plane.
  @end{short}
  @see-symbol{plane-t}"
  (plane (:pointer (:struct plane-t)))
  (source (:pointer (:struct plane-t))))

(export 'plane-init-from-plane)

;;; ----------------------------------------------------------------------------
;;; graphene_plane_init_from_point ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_plane_init_from_point" plane-init-from-point)
    (:pointer (:struct plane-t))
 #+liber-documentation
 "@version{#2022-9-30}
  @argument[plane]{a @symbol{plane-t} instance to initialize}
  @argument[normal]{a @symbol{vec3-t} instance with the normal vector
    defining the plane pointing towards the orign}
  @argument[point]{a @symbol{point3d-t} instance}
  @return{A initialized @symbol{plane-t} instance.}
  @begin{short}
    Initializes the given plane using the given normal vector and an arbitrary
    co-planar point.
  @end{short}
  @see-symbol{plane-t}
  @see-symbol{vec3-t}
  @see-symbol{point3d-t}"
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
 "@version{#2022-9-30}
  @argument[plane]{a @symbol{plane-t} instance to initialize}
  @argument[a]{a @symbol{point3d-t} instance}
  @argument[b]{a @symbol{point3d-t} instance}
  @argument[c]{a @symbol{point3d-t} instance}
  @return{A initialized @symbol{plane-t} instance.}
  @begin{short}
    Initializes the given plane using the 3 provided co-planar points.
  @end{short}
  The winding order is counter-clockwise, and determines which direction the
  normal vector will point.
  @see-symbol{plane-t}
  @see-symbol{point3d-t}"
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
 "@version{#2022-9-30}
  @argument[plane]{a @symbol{plane-t} instance to initialize}
  @argument[result]{a @symbol{plane-t} instance for the result}
  @return{A normalized @symbol{plane-t} instance.}
  @begin{short}
    Normalizes the vector of the given plane, and adjusts the constant
    accordingly.
  @end{short}
  @see-symbol{plane-t}"
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
 "@version{#2022-9-30}
  @argument[plane]{a @symbol{plane-t} instance to initialize}
  @argument[result]{a @symbol{plane-t} instance for the result}
  @return{A normalized @symbol{plane-t} instance.}
  @begin{short}
    Negates the normal vector and constant of the plane, effectively mirroring
    the plane across the origin.
  @end{short}
  @see-symbol{plane-t}"
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
 "@version{#2022-9-30}
  @argument[a]{a @symbol{plane-t} instance}
  @argument[b]{a @symbol{plane-t} instance}
  @return{@em{True} if the given planes are equal.}
  @begin{short}
    Checks whether the two given planes are equal.
  @end{short}
  @see-symbol{plane-t}"
  (a (:pointer (:struct plane-t)))
  (b (:pointer (:struct plane-t))))

(export 'plane-equal)

;;; ----------------------------------------------------------------------------
;;; graphene_plane_distance ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_plane_distance" plane-distance) :float
 #+liber-documentation
 "@version{#2022-9-30}
  @argument[plane]{a @symbol{plane-t} instance}
  @argument[point]{a @symbol{point3d-t} instance}
  @return{A single float with the distance of the given point from the plane.}
  @begin{short}
    Computes the distance of the point from the plane.
  @end{short}
  @see-symbol{plane-t}"
  (plane (:pointer (:struct plane-t)))
  (point (:pointer (:struct point3d-t))))

(export 'plane-distance)

;;; ----------------------------------------------------------------------------
;;; graphene_plane_transform ()
;;; ----------------------------------------------------------------------------

(defun plane-transform (plane matrix normal result)
 #+liber-documentation
 "@version{#2022-9-30}
  @argument[plane]{a @symbol{plane-t} instance}
  @argument[matrix]{a @symbol{matrix-t} instance}
  @argument[normal]{a @symbol{matrix-t} instance}
  @argument[result]{a @symbol{plane-t} instance}
  @return{A transformed @symbol{plant-t} instance.}
  @begin{short}
    Transforms a plane using the given matrix and normal matrix.
  @end{short}
  If the normal matrix is @code{nil}, a transformation matrix for the plane
  normal will be computed from @arg{matrix}. If you are transforming multiple
  planes using the same matrix it is recommended to compute the normal matrix
  beforehand to avoid incurring in the cost of recomputing it every time.
  @see-symbol{plane-t}
  @see-symbol{matrix-t}"
  (cffi:foreign-funcall "graphene_plane_transform"
                        (:pointer (:struct plane-t)) plane
                        (:pointer (:struct matrix-t)) matrix
                        (:pointer (:struct matrix-t)) (if normal
                                                          normal
                                                          (cffi:null-pointer))
                        (:pointer (:struct plane-t)) result
                        :void)
  result)

(export 'plane-transform)

;;; ----------------------------------------------------------------------------
;;; graphene_plane_get_normal ()
;;; ----------------------------------------------------------------------------

(defun plane-normal (plane normal)
 #+liber-documentation
 "@version{#2022-9-30}
  @argument[plane]{a @symbol{plane-t} instance}
  @argument[normal]{a @symbol{vec3-t} instance for the normal vector}
  @return{A @symbol{vec3-t} instance with the normal vector.}
  @begin{short}
    Retrieves the normal vector pointing towards the origin of the given
    plane.
  @end{short}
  @see-symbol{plane-t}
  @see-symbol{vec3-t}"
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
 "@version{#2022-9-30}
  @argument[plane]{a @symbol{plane-t} instance}
  @return{A single float with the constant value of the plane.}
  @begin{short}
    Retrieves the distance along the normal vector of the given plane from the
    origin.
  @end{short}
  @see-symbol{plane-t}"
  (plane (:pointer (:struct plane-t))))

(export 'plane-constant)

;;; --- End of file graphene.plane.lisp ----------------------------------------
