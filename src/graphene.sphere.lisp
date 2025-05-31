;;; ----------------------------------------------------------------------------
;;; graphene.sphere.lisp
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
;;; Sphere
;;;
;;;     A sphere
;;;
;;; Types and Values
;;;
;;;     graphene_sphere_t
;;;
;;; Functions
;;;
;;;     graphene_sphere_alloc
;;;     graphene_sphere_free
;;;     graphene_sphere_init
;;;     graphene_sphere_init_from_points
;;;     graphene_sphere_init_from_vectors
;;;     graphene_sphere_get_center
;;;     graphene_sphere_get_radius
;;;     graphene_sphere_get_bounding_box
;;;     graphene_sphere_is_empty
;;;     graphene_sphere_distance
;;;     graphene_sphere_contains_point
;;;     graphene_sphere_translate
;;;     graphene_sphere_equal
;;; ----------------------------------------------------------------------------

(in-package :graphene)

(defmacro with-sphere ((var &rest args) &body body)
 #+liber-documentation
 "@version{2025-05-09}
  @syntax{(graphene:with-sphere (sphere) body) => result}
  @syntax{(graphene:with-sphere (sphere sphere1) body) => result}
  @syntax{(graphene:with-sphere (sphere center radius) body) => result}
  @argument[sphere]{a @symbol{graphene:sphere-t} instance to create and
    initialize}
  @argument[sphere1]{a @symbol{graphene:sphere-t} instance to use for
    initialization}
  @argument[center]{a @symbol{graphene:point3d-t} instance to use for
    initialization}
  @argument[radius]{a single float for the radius of the sphere}
  @begin{short}
    The @fun{graphene:with-sphere} macro allocates a new
    @symbol{graphene:sphere-t} instance, initializes the sphere with the given
    values and executes the body that uses the sphere.
  @end{short}
  After execution of the body the allocated memory for the sphere is released.

  If no argument is given, the components of the sphere are initialized to
  zero. The initialization from another sphere is done with the
  @fun{graphene:sphere-init-from-sphere} function. The initialization with
  a center and a radius uses the @fun{graphene:sphere-init} function.
  @begin[Notes]{dictionary}
    The memory is allocated with the @fun{graphene:sphere-alloc} function and
    released with the @fun{graphene:sphere-free} function.
  @end{dictionary}
  @see-symbol{graphene:sphere-t}
  @see-symbol{graphene:point3d-t}
  @see-macro{graphene:with-spheres}
  @see-function{graphene:sphere-alloc}
  @see-function{graphene:sphere-free}"
  (cond ((null args)
         ;; No arguments, the default is initialization with zeros
         `(let ((,var (sphere-alloc)))
            (sphere-init ,var (point3d-zero) 0.0)
            (unwind-protect
              (progn ,@body)
              (sphere-free ,var))))
        ((null (second args))
         ;; One argument of type sphere-t
         (dbind (arg1 &optional type1 &rest rest1) (mklist (first args))
           (declare (ignore rest1))
           (cond ((eq type1 'sphere-t)
                  ;; One argument of type sphere-t
                  `(let ((,var (sphere-alloc)))
                     (sphere-init-from-sphere ,var ,arg1)
                     (unwind-protect
                       (progn ,@body)
                       (sphere-free ,var))))
                 (t
                  ;; One argument with no type, default is sphere-t
                  `(let ((,var (sphere-alloc)))
                     (sphere-init-from-sphere ,var ,@args)
                     (unwind-protect
                       (progn ,@body)
                       (sphere-free ,var)))))))
        ((null (third args))
         ;; Two arguments. The first must be of type point3d-t and
         ;; the second a single float.
         (dbind (arg1 &optional type1 &rest rest1) (mklist (first args))
           (declare (ignore rest1))
           (cond ((eq type1 'point3d-t)
                  ;; First argument of type point3d-t
                  `(let ((,var (sphere-alloc)))
                     (sphere-init ,var ,arg1 ,@(rest args))
                     (unwind-protect
                       (progn ,@body)
                       (sphere-free ,var))))
                 (t
                  ;; No type for first argument, default is point3d-t
                  `(let ((,var (sphere-alloc)))
                     (sphere-init ,var ,@args)
                     (unwind-protect
                       (progn ,@body)
                       (sphere-free ,var)))))))
        (t
         (error "Syntax error in GRAPHENE:WITH-SPHERE"))))

(export 'with-sphere)

(defmacro with-spheres (vars &body body)
 #+liber-documentation
 "@version{2025-4-5}
  @syntax{(graphene:with-spheres (sphere1 sphere2 ... spheren) body) => result}
  @argument[sphere1 ... spheren]{newly created @symbol{graphene:sphere-t}
    instances}
  @argument[body]{a body that uses the bindings @arg{sphere1 ... spheren}}
  @begin{short}
    The @fun{graphene:with-spheres} macro creates new variable bindings and
    executes the body that use these bindings.
  @end{short}
  The macro performs the bindings sequentially, like the @sym{let*} macro.

  Each point can be initialized with values using the syntax for the
  @fun{graphene:with-sphere} macro. See also the @fun{graphene:with-sphere}
  documentation.
  @see-symbol{graphene:sphere-t}
  @see-macro{graphene:with-sphere}"
  (if vars
      (let ((var (if (listp (first vars)) (first vars) (list (first vars)))))
        `(with-sphere ,var
           (with-spheres ,(rest vars)
             ,@body)))
      `(progn ,@body)))

(export 'with-spheres)

;;; ----------------------------------------------------------------------------
;;; graphene_sphere_t
;;; ----------------------------------------------------------------------------

(cffi:defcstruct sphere-t)

#+liber-documentation
(setf (liber:alias-for-symbol 'sphere-t)
      "CStruct"
      (liber:symbol-documentation 'sphere-t)
 "@version{2025-4-5}
  @begin{declaration}
(cffi:defcstruct sphere-t)
  @end{declaration}
  @begin{short}
    The @symbol{graphene:sphere-t} structure provides a representation of a
    sphere using its center and radius.
  @end{short}")

(export 'sphere-t)

;;; ----------------------------------------------------------------------------
;;; graphene_sphere_alloc
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_sphere_alloc" sphere-alloc)
    (:pointer (:struct sphere-t))
 #+liber-documentation
 "@version{2025-4-5}
  @return{The newly allocated @symbol{graphene:sphere-t} instance.}
  @begin{short}
    Allocates a new @symbol{graphene:sphere-t} instance.
  @end{short}
  The contents of the returned instance are undefined. Use the
  @fun{graphene:sphere-free} function to free the resources allocated by this
  function.
  @see-symbol{graphene:sphere-t}
  @see-function{graphene:sphere-free}")

(export 'sphere-alloc)

;;; ----------------------------------------------------------------------------
;;; graphene_sphere_free
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_sphere_free" sphere-free) :void
 #+liber-documentation
 "@version{2025-4-5}
  @argument[sphere]{a @symbol{graphene:sphere-t} instance}
  @begin{short}
    Frees the resources allocated by the @fun{graphene:sphere-alloc} function.
  @end{short}
  @see-symbol{graphene:sphere-t}
  @see-function{graphene:sphere-alloc}"
  (sphere (:pointer (:struct sphere-t))))

(export 'sphere-free)

;;; ----------------------------------------------------------------------------
;;; graphene_sphere_init
;;; ----------------------------------------------------------------------------

(defun sphere-init (sphere center radius)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[sphere]{a @symbol{graphene:sphere-t} instance to initialize}
  @argument[center]{a @symbol{graphene:point3d-t} instance for the coordinates
    of the center of the sphere}
  @argument[radius]{a number coerced to a single float for the radius of the
    sphere}
  @return{The initialized @symbol{graphene:sphere-t} instance.}
  @begin{short}
    Initializes the given @symbol{graphene:sphere-t} instance with the given
    center and radius.
  @end{short}
  @see-symbol{graphene:sphere-t}
  @see-symbol{graphene:point3d-t}"
  (cffi:foreign-funcall "graphene_sphere_init"
                        (:pointer (:struct sphere-t)) sphere
                        (:pointer (:struct point3d-t)) center
                        :float (coerce radius 'single-float)
                        (:pointer (:struct sphere-t))))

(export 'sphere-init)

;;; ----------------------------------------------------------------------------
;;; graphene_sphere_init_from_points
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_sphere_init_from_points" %sphere-init-from-points)
    (:pointer (:struct sphere-t))
  (sphere (:pointer (:struct sphere-t)))
  (n-points :uint)
  (points (:pointer (:struct point3d-t)))
  (center (:pointer (:struct point3d-t))))

(defun sphere-init-from-points (sphere points center)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[sphere]{a @symbol{graphene:sphere-t} instance to initialize}
  @argument[points]{a list of @symbol{graphene:point3d-t} instances}
  @argument[center]{a @symbol{graphne:point3d-t} instance for the center of
    the sphere, or @code{nil}}
  @return{The initialized @symbol{graphene:sphere-t} instance.}
  @begin{short}
    Initializes the given sphere using the given list of 3D coordinates so that
    the sphere includes them.
  @end{short}
  The center of the sphere can either be specified, or will be center of the
  3D volume that encompasses all points.
  @see-symbol{graphene:sphere-t}
  @see-symbol{graphene:point3d-t}"
  (let ((n-points (length points))
        (center (or center (cffi:null-pointer))))
    (cffi:with-foreign-object (points-ar '(:struct point3d-t) n-points)
      (iter (for i from 0 below n-points)
            (for point in points)
            (for ptr = (cffi:mem-aptr points-ar '(:struct point3d-t) i))
            (point3d-init-from-point ptr point))
      (%sphere-init-from-points sphere n-points points-ar center))))

(export 'sphere-init-from-points)

;;; ----------------------------------------------------------------------------
;;; graphene_sphere_init_from_vectors
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_sphere_init_from_vectors" %sphere-init-from-vectors)
    (:pointer (:struct sphere-t))
  (sphere (:pointer (:struct sphere-t)))
  (n-vectors :uint)
  (vectors (:pointer (:struct vec3-t)))
  (center (:pointer (:struct point3d-t))))

(defun sphere-init-from-vectors (sphere vectors center)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[sphere]{a @symbol{graphene:sphere-t} instance to initialize}
  @argument[vectors]{a list of @symbol{graphene:vec3-t} instances}
  @argument[center]{a @symbol{graphne:point3d-t} instance for the center of
    the sphere, or @code{nil}}
  @return{The initialized @symbol{graphene:sphere-t} instance.}
  @begin{short}
    Initializes the given sphere using the given list of 3D coordinates so that
    the sphere includes them.
  @end{short}
  The center of the sphere can either be specified, or will be center of the
  3D volume that encompasses all vectors.
  @see-symbol{graphene:sphere-t}
  @see-symbol{graphene:vec3-t}
  @see-symbol{graphene:point3d-t}"
  (let ((n-vectors (length vectors))
        (center (or center (cffi:null-pointer))))
    (cffi:with-foreign-object (vectors-ar '(:struct vec3-t) n-vectors)
      (iter (for i from 0 below n-vectors)
            (for vec in vectors)
            (for ptr = (cffi:mem-aptr vectors-ar '(:struct vec3-t) i))
            (vec3-init-from-vec3 ptr vec))
      (%sphere-init-from-vectors sphere n-vectors vectors-ar center))))

(export 'sphere-init-from-vectors)

;;; ----------------------------------------------------------------------------
;;; graphene:sphere-init-from-sphere
;;; ----------------------------------------------------------------------------

(defun sphere-init-from-sphere (sphere source)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[sphere]{a @symbol{graphene:sphere-t} instance to initialize}
  @argument[source]{a @symbol{graphene:sphere-t} instance for initialization}
  @return{The initialized @symbol{graphene:sphere-t} instance.}
  @short{Initialize a sphere from another sphere.}
  @begin[Notes]{dictionary}
    This implementation is added to the Lisp library and not present in the
    Graphene C library.
  @end{dictionary}
  @see-symbol{graphene:sphere-t}"
  (graphene:with-point3d (center)
    (sphere-init sphere (sphere-center source center) (sphere-radius source))))

(export 'sphere-init-from-sphere)

;;; ----------------------------------------------------------------------------
;;; graphene_sphere_get_center
;;; ----------------------------------------------------------------------------

(defun sphere-center (sphere center)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[sphere]{a @symbol{graphene:sphere-t} instance}
  @argument[center]{a @symbol{graphene:point3d-t} instance for the coordinates
    of the center}
  @begin{return}
    The @symbol{graphene:point3d-t} instance with the coordinates of the
    center.
  @end{return}
  @short{Retrieves the coordinates of the center of a sphere.}
  @see-symbol{graphene:sphere-t}
  @see-symbol{graphene:point3d-t}"
  (cffi:foreign-funcall "graphene_sphere_get_center"
                        (:pointer (:struct sphere-t)) sphere
                        (:pointer (:struct point3d-t)) center
                        :void)
  center)

(export 'sphere-center)

;;; ----------------------------------------------------------------------------
;;; graphene_sphere_get_radius
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_sphere_get_radius" sphere-radius) :float
 #+liber-documentation
 "@version{2025-4-5}
  @argument[sphere]{a @symbol{graphene:sphere-t} instance}
  @return{The single float with the radius.}
  @short{Retrieves the radius of a sphere.}
  @see-symbol{graphene:sphere-t}"
  (sphere (:pointer (:struct sphere-t))))

(export 'sphere-radius)

;;; ----------------------------------------------------------------------------
;;; graphene_sphere_get_bounding_box
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_sphere_get_bounding_box" %sphere-bounding-box) :void
  (sphere (:pointer (:struct sphere-t)))
  (box (:pointer (:struct box-t))))

(defun sphere-bounding-box (sphere box)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[sphere]{a @symbol{graphene:sphere-t} instance}
  @argument[box]{a @symbol{graphene:box-t} instance for the bounding box}
  @return{The @symbol{graphene:box-t} instance with the bounding box.}
  @short{Computes the bounding box capable of containing the given sphere.}
  @see-symbol{graphene:sphere-t}
  @see-symbol{graphene:box-t}"
  (%sphere-bounding-box sphere box)
  box)

(export 'sphere-bounding-box)

;;; ----------------------------------------------------------------------------
;;; graphene_sphere_is_empty
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_sphere_is_empty" sphere-is-empty) :bool
 #+liber-documentation
 "@version{2025-4-5}
  @argument[sphere]{a @symbol{graphene:sphere-t} instance}
  @return{@em{True} if the sphere is empty.}
  @short{Checks whether the sphere has a zero radius.}
  @see-symbol{graphene:sphere-t}"
  (sphere (:pointer (:struct sphere-t))))

(export 'sphere-is-empty)

;;; ----------------------------------------------------------------------------
;;; graphene_sphere_distance
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_sphere_distance" sphere-distance) :float
 #+liber-documentation
 "@version{2025-4-5}
  @argument[sphere]{a @symbol{graphene:sphere-t} instance}
  @argument[point]{a @symbol{graphene:point3d-t} instance}
  @return{The single float with the distance of the point.}
  @begin{short}
    Computes the distance of the given point from the surface of a sphere.
  @end{short}
  @see-symbol{graphene:sphere-t}
  @see-symbol{graphene:point3d-t}"
  (sphere (:pointer (:struct sphere-t)))
  (point (:pointer (:struct point3d-t))))

(export 'sphere-distance)

;;; ----------------------------------------------------------------------------
;;; graphene_sphere_contains_point
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_sphere_contains_point" sphere-contains-point) :bool
 #+liber-documentation
 "@version{2025-4-5}
  @argument[sphere]{a @symbol{graphene:sphere-t} instance}
  @argument[point]{a @symbol{graphene:point3d-t} instance}
  @return{@em{True} if the sphere contains the point.}
  @begin{short}
    Checks whether the given point is contained in the volume of a sphere.
  @end{short}
  @see-symbol{graphene:sphere-t}
  @see-symbol{graphene:point3d-t}"
  (sphere (:pointer (:struct sphere-t)))
  (point (:pointer (:struct point3d-t))))

(export 'sphere-contains-point)

;;; ----------------------------------------------------------------------------
;;; graphene_sphere_translate
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_sphere_translate" %sphere-translate) :void
  (sphere (:pointer (:struct sphere-t)))
  (point (:pointer (:struct point3d-t)))
  (result (:pointer (:struct sphere-t))))

(defun sphere-translate (sphere point result)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[sphere]{a @symbol{graphene:sphere-t} instance}
  @argument[point]{a @symbol{graphene:point3d-t} instance for the deltas of
    the translation}
  @argument[result]{a @symbol{graphene:sphere-t} instance for the translated
    sphere}
  @return{The @symbol{graphene:sphere-t} instance with the translated sphere.}
  @begin{short}
    Translates the center of the given sphere using the point coordinates as
    the delta of the translation.
  @end{short}
  @see-symbol{graphene:sphere-t}
  @see-symbol{graphene:point3d-t}"
  (%sphere-translate sphere point result)
  result)

(export 'sphere-translate)

;;; ----------------------------------------------------------------------------
;;; graphene_sphere_equal
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_sphere_equal" sphere-equal) :bool
 #+liber-documentation
 "@version{2025-4-5}
  @argument[a]{a @symbol{graphene:sphere-t} instance}
  @argument[b]{a @symbol{graphene:sphere-t} instance}
  @return{@em{True} if the spheres are equal.}
  @short{Checks whether two spheres are equal.}
  @see-symbol{graphene:sphere-t}"
  (a (:pointer (:struct sphere-t)))
  (b (:pointer (:struct sphere-t))))

(export 'sphere-equal)

;;; --- End of file graphene.sphere.lisp ---------------------------------------
