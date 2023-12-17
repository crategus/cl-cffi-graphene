;;; ----------------------------------------------------------------------------
;;; graphene.ray.lisp
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
;;; Ray
;;;
;;;     A ray emitted from an origin in a given direction
;;;
;;; Types and Values
;;;
;;;     graphene_ray_t
;;;     graphene_ray_intersection_kind_t
;;;
;;; Functions
;;;
;;;     graphene_ray_alloc
;;;     graphene_ray_free
;;;     graphene_ray_init
;;;     graphene_ray_init_from_ray
;;;     graphene_ray_init_from_vec3
;;;     graphene_ray_get_origin
;;;     graphene_ray_get_direction
;;;     graphene_ray_get_position_at
;;;     graphene_ray_get_distance_to_point
;;;     graphene_ray_get_distance_to_plane
;;;     graphene_ray_get_closest_point_to_point
;;;     graphene_ray_equal
;;;     graphene_ray_intersect_sphere
;;;     graphene_ray_intersects_sphere
;;;     graphene_ray_intersect_box
;;;     graphene_ray_intersects_box
;;;     graphene_ray_intersect_triangle
;;;     graphene_ray_intersects_triangle
;;; ----------------------------------------------------------------------------

(in-package :graphene)

(defmacro with-ray ((var &rest args) &body body)
 #+liber-documentation
 "@version{2023-12-10}
  @syntax[]{(graphene:with-ray (ray) body) => result}
  @syntax[]{(graphene:with-ray (ray ray1) body) => result}
  @syntax[]{(graphene:with-ray (ray origin direction) body) => result}
  @syntax[]{(graphene:with-ray (ray (origin graphene:vec3-t) direction) body)
    => result}
  @argument[ray]{a @symbol{graphene:ray-t} instance to create and initialize}
  @argument[ray1]{a @symbol{graphene:ray-t} instance to use for initialization}
  @argument[origin]{a @symbol{graphene:point3d-t} or a @symbol{graphene:vec3-t}
    instance to use for initialization}
  @argument[direction]{a @symbol{graphene:vec3-t} instance to use for
    initialization}
  @begin{short}
    The @fun{graphene:with-ray} macro allocates a new @symbol{graphene:ray-t}
    instance, initializes the box with the given values and executes the body
    that uses the ray.
  @end{short}
  After execution of the body the allocated memory for the box is released.
  @begin[Note]{dictionary}
    The memory is allocated with the @fun{graphene:ray-alloc} function and
    released with the @fun{graphene:ray-free} function.
  @end{dictionary}
  @see-symbol{graphene:ray-t}
  @see-symbol{graphene:point3d-t}
  @see-symbol{grapene:vec3-t}
  @see-macro{graphene:with-rays}
  @see-function{graphene:ray-alloc}
  @see-function{graphene:ray-free}"
  (cond ((null args)
         ;; We have no arguments, the default is no initialization
         `(let ((,var (ray-alloc)))
            (unwind-protect
              (progn ,@body)
              (ray-free ,var))))
        ((null (second args))
         ;; We have one argument. The argument must be of type ray-t.
         (destructuring-bind (arg &optional type1) (mklist (first args))
           (cond ((or (not type1)
                      (eq type1 'ray-t))
                  ;; One argument with no type or of type ray-t
                  `(let ((,var (ray-alloc)))
                     (ray-init-from-ray ,var ,arg)
                     (unwind-protect
                       (progn ,@body)
                       (ray-free ,var))))
                 (t
                  (error "Syntax error in GRAPHENE:WITH-RAY")))))
        ((null (third args))
         ;; We have two arguments. The first can be of type point3d-t or
         ;; vec3-t. The second argument must be of type vec3-t
         (destructuring-bind ((arg1 &optional type1)
                              (arg2 &optional type2))
             (list (mklist (first args)) (mklist (second args)))
           (cond ((and (or (not type1)
                           (eq type1 'point3d-t))
                       (or (not type2)
                           (eq type2 'vec3-t)))
                  ;; First argument with no type or of type point3d-t and
                  ;; second argument with no type or type vec3-t
                  `(let ((,var (ray-alloc)))
                     (ray-init ,var ,arg1 ,arg2)
                     (unwind-protect
                       (progn ,@body)
                       (ray-free ,var))))
                 ((and (eq type1 'vec3-t)
                       (or (not type2)
                           (eq type2 'vec3-t)))
                  ;; First argument of type vec3-t and second argument with
                  ;; no type or type vec3-t
                  `(let ((,var (ray-alloc)))
                     (ray-init-from-vec3 ,var ,arg1 ,arg2)
                     (unwind-protect
                       (progn ,@body)
                       (ray-free ,var))))
                 (t
                  (error "Syntax error in GRAPHENE:WITH-RAY")))))
        (t
         (error "Syntax error in GRAPHENE:WITH-RAY"))))

(export 'with-ray)

(defmacro with-rays (vars &body body)
 #+liber-documentation
 "@version{2023-12-10}
  @syntax[]{(graphene:with-rays (ray1 ... rayn) body) => result}
  @argument[ray1 ... rayn]{the newly created @symbol{graphene:ray-t} instances}
  @argument[body]{a body that uses the bindings @arg{ray1 ... rayn}}
  @begin{short}
    The @fun{graphene:with-rays} macro creates new variable bindings and
    executes the body that use these bindings.
  @end{short}
  The macro performs the bindings sequentially, like the @sym{let*} macro.

  Each ray can be initialized with values using the syntax for the
  @fun{graphene:with-ray} macro. See also the @fun{graphene:with-ray}
  documentation.
  @see-symbol{graphene:box-t}
  @see-macro{graphene:with-box}"
  (if vars
      (let ((var (mklist (first vars))))
        `(with-ray ,var
           (with-rays ,(rest vars)
             ,@body)))
      `(progn ,@body)))

(export 'with-rays)

;;; ----------------------------------------------------------------------------
;;; graphene_ray_intersection_kind_t
;;; ----------------------------------------------------------------------------

(cffi:defcenum ray-intersection-kind-t
  :none
  :enter
  :leave)

#+liber-documentation
(setf (liber:alias-for-symbol 'ray-intersection-kind-t)
      "CEnum"
      (liber:symbol-documentation 'ray-intersection-kind-t)
 "@version{#2023-12-5}
  @short{The type of intersection.}
  @begin{pre}
(cffi:defcenum ray-intersection-kind-t
  :none
  :enter
  :leave)
  @end{pre}
  @begin[code]{table}
    @entry[:none]{No intersection.}
    @entry[:enter]{The ray is entering the intersected object.}
    @entry[:leave]{The ray is leaving the intersected object.}
  @end{table}
  @see-symbol{graphene:ray-t}")

(export 'ray-intersection-kind-t)

;;; ----------------------------------------------------------------------------
;;; graphene_ray_t
;;; ----------------------------------------------------------------------------

(cffi:defcstruct ray-t)

#+liber-documentation
(setf (liber:alias-for-symbol 'ray-t)
      "CStruct"
      (liber:symbol-documentation 'ray-t)
 "@version{2023-12-5}
  @begin{short}
    The @symbol{graphene:ray-t} structure is a structure representing a ray
    emitted by an origin, identified by a point in 3D space, in a given
    direction, identified by a vector with 3 components.
  @end{short}

  A common use of the @symbol{graphene:ray-t} implementation is ray-casting to
  find which objects in a 3D scene are under the coordinates of the pointer.")

(export 'ray-t)

;;; ----------------------------------------------------------------------------
;;;graphene_ray_alloc ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_ray_alloc" ray-alloc)
    (:pointer (:struct ray-t))
 #+liber-documentation
 "@version{2023-12-5}
  @return{The newly allocated @symbol{graphene:ray-t} instance.}
  @begin{short}
    Allocates a new @symbol{graphene:ray-t} instance.
  @end{short}
  The contents of the returned structure are undefined. Use the
    @fun{graphene:ray-free} function to free the resources allocated by this
    function.
  @see-symbol{graphene:ray-t}
  @see-function{graphene:ray-free}")

(export 'ray-alloc)

;;; ----------------------------------------------------------------------------
;;;graphene_ray_free ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_ray_free" ray-free) :void
 #+liber-documentation
 "@version{2023-12-5}
  @argument[ray]{a @symbol{graphene:ray-t} instance}
  @begin{short}
    Frees the resources allocated by the @fun{graphene:ray-alloc} function.
  @end{short}
  @see-symbol{graphene:ray-t}
  @see-function{graphene:ray-alloc}"
  (ray (:pointer (:struct point-t))))

(export 'ray-free)

;;; ----------------------------------------------------------------------------
;;;graphene_ray_init ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_ray_init" ray-init) (:pointer (:struct ray-t))
 #+liber-documentation
 "@version{2023-12-5}
  @argument[ray]{a @symbol{graphene:ray-t} instance to be initialized}
  @argument[origin]{a @symbol{graphene:point3d-t} instance with the origin of
    the ray}
  @argument[direction]{a @symbol{graphene:vec3-t} instance with the direction
    vector}
  @return{The initialized @symbol{graphene:ray-t} instance.}
  @begin{short}
    Initializes the given @symbol{graphene:ray-t} instance using the given
    origin and direction values.
  @end{short}
  The direction vector is normalized.
  @see-symbol{graphene:ray-t}
  @see-symbol{graphene:point3d-t}
  @see-symbol{graphene:vec3-t}"
  (ray (:pointer (:struct ray-t)))
  (origin (:pointer (:struct point3d-t)))
  (direction (:pointer (:struct vec3-t))))

(export 'ray-init)

;;; ----------------------------------------------------------------------------
;;; graphene_ray_init_from_ray ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_ray_init_from_ray" ray-init-from-ray)
    (:pointer (:struct ray-t))
 #+liber-documentation
 "@version{2023-12-5}
  @argument[ray]{a @symbol{graphene:ray-t} instance to initialize}
  @argument[source]{a @symbol{graphene:ray-t} instance}
  @return{The initialized @symbol{graphene:ray-t} instance.}
  @begin{short}
    Initializes the given ray using the origin and direction values of another
    ray.
  @end{short}
  @see-symbol{graphene:ray-t}"
  (ray (:pointer (:struct ray-t)))
  (source (:pointer (:struct ray-t))))

(export 'ray-init-from-ray)

;;; ----------------------------------------------------------------------------
;;; graphene_ray_init_from_vec3 ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_ray_init_from_vec3" ray-init-from-vec3)
    (:pointer (:struct ray-t))
 #+liber-documentation
 "@version{2023-12-5}
  @argument[ray]{a @symbol{graphene:ray-t} instance to initialize}
  @argument[origin]{a @symbol{graphene:vec3-t} instance}
  @argument[direction]{a @symbol{graphene:vec3-t}_instance}
  @return{The initialized @symbol{graphene:ray-t} instance.}
  @short{Initializes the given ray using the given vectors.}
  @see-symbol{graphene:ray-t}
  @see-symbol{graphene:vec3-t}"
  (ray (:pointer (:struct ray-t)))
  (origin (:pointer (:struct vec3-t)))
  (direction (:pointer (:struct vec3-t))))

(export 'ray-init-from-vec3)

;;; ----------------------------------------------------------------------------
;;; graphene_ray_get_origin ()
;;; ----------------------------------------------------------------------------

(defun ray-origin (ray origin)
 #+liber-documentation
 "@version{2023-12-5}
  @argument[ray]{a @symbol{graphene:ray-t} instance}
  @argument[origin]{a @symbol{graphene:point3d-t} instance for the origin}
  @return{The @symbol{graphene:point3d-t} instance with the origin.}
  @short{Retrieves the origin of the given ray.}
  @see-symbol{graphene:ray-t}
  @see-symbol{graphene:point3d-t}"
  (cffi:foreign-funcall "graphene_ray_get_origin"
                        (:pointer (:struct ray-t)) ray
                        (:pointer (:struct point3d-t)) origin
                        :void)
  origin)

(export 'ray-origin)

;;; ----------------------------------------------------------------------------
;;; graphene_ray_get_direction ()
;;; ----------------------------------------------------------------------------

(defun ray-direction (ray direction)
 #+liber-documentation
 "@version{2023-12-5}
  @argument[ray]{a @symbol{graphene:ray-t} instance}
  @argument[direction]{a @symbol{graphene:vec3-t} instance for the direction}
  @return{The @symbol{graphene:vec3-t} instance with the direction.}
  @short{Retrieves the direction of the given ray.}
  @see-symbol{graphene:ray-t}
  @see-symbol{graphene:vec3-t}"
  (cffi:foreign-funcall "graphene_ray_get_direction"
                        (:pointer (:struct ray-t)) ray
                        (:pointer (:struct vec3-t)) direction
                        :void)
  direction)

(export 'ray-direction)

;;; ----------------------------------------------------------------------------
;;; graphene_ray_get_position_at ()
;;; ----------------------------------------------------------------------------

(defun ray-position-at (ray parameter position)
 #+liber-documentation
 "@version{2023-12-5}
  @argument[ray]{a @symbol{graphene:ray-t} instance}
  @argument[parameter]{a number coerced to a float with the distance along the
    ray}
  @argument[position]{a @symbol{graphene:point3d-t} instance for the position}
  @return{The @symbol{graphene:point3d-t} instance with the position}
  @begin{short}
    Retrieves the coordinates of a point at the distance @arg{parameter} along
    the given ray.
  @end{short}
  @see-symbol{graphene:ray-t}
  @see-symbol{graphene:point3d-t}"
  (cffi:foreign-funcall "graphene_ray_get_position_at"
                        (:pointer (:struct ray-t)) ray
                        :float (coerce parameter 'single-float)
                        (:pointer (:struct point3d-t)) position
                        :void)
  position)

(export 'ray-position-at)

;;; ----------------------------------------------------------------------------
;;; graphene_ray_get_distance_to_point ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_ray_get_distance_to_point" ray-distance-to-point)
    :float
 #+liber-documentation
 "@version{2023-12-5}
  @argument[ray]{a @symbol{graphene:ray-t} instance}
  @argument[point]{a @symbol{graphene:point3d-t} instance}
  @return{The float with the distance of the point.}
  @begin{short}
    Computes the distance of the closest approach between the given ray and the
    point.
  @end{short}
  The closest approach to a ray from a point is the distance between the point
  and the projection of the point on the ray itself.
  @see-symbol{graphene:ray-t}
  @see-symbol{graphene:point3d-t}"
  (ray (:pointer (:struct ray-t)))
  (point (:pointer (:struct point3d-t))))

(export 'ray-distance-to-point)

;;; ----------------------------------------------------------------------------
;;; graphene_ray_get_distance_to_plane ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_ray_get_distance_to_plane" ray-distance-to-plane)
    :float
 #+liber-documentation
 "@version{#2023-12-6}
  @argument[ray]{a @symbol{graphene:ray-t} instance}
  @argument[plane]{a @symbol{graphene:plane-t} instance}
  @return{The float with the distance of the origin of the ray from the plane}
  @begin{short}
    Computes the distance of the origin of the given ray from the given plane.
  @end{short}
  If the ray does not intersect the plane, this function returns
  @code{INFINITY}.
  @see-symbol{graphene:ray-t}
  @see-symbol{graphene:plane-t}"
  (ray (:pointer (:struct ray-t)))
  (plane (:pointer (:struct plane-t))))

(export 'ray-distance-to-plane)

;;; ----------------------------------------------------------------------------
;;; graphene_ray_get_closest_point_to_point ()
;;; ----------------------------------------------------------------------------

(defun ray-closest-point-to-point (ray point result)
 #+liber-documentation
 "@version{#2023-12-6}
  @argument[ray]{a @symbol{graphene:ray-t} instance}
  @argument[point]{a @symbol{graphene:point3d-t} instance}
  @argument[result]{a @symbol{graphene:point3d-t} instance for the result}
  @return{The @symbol{graphene:point3d-t} instance with the closest point.}
  @begin{short}
    Computes the point on the given ray that is closest to the given point.
  @end{short}
  @see-symbol{graphene:ray-t}
  @see-symbol{graphene:point3d-t}"
  (cffi:foreign-funcall "graphene_ray_get_closest_point_to_point"
                        (:pointer (:struct ray-t)) ray
                        (:pointer (:struct point3d-t)) point
                        (:pointer (:struct point3d-t)) result
                        :void)
  result)

(export 'ray-closest-point-to-point)

;;; ----------------------------------------------------------------------------
;;; graphene_ray_equal ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_ray_equal" ray-equal) :bool
 #+liber-documentation
 "@version{#2023-12-6}
  @argument[a]{a @symbol{graphene:ray-t} instance}
  @argument[b]{a @symbol{graphene:ray-t} instance}
  @return{@em{True} if the given are equal.}
  @short{Checks whether the two given rays are equal.}
  @see-symbol{graphene:ray-t}"
  (a (:pointer (:struct ray-t)))
  (b (:pointer (:struct ray-t))))

(export 'ray-equal)

;;; ----------------------------------------------------------------------------
;;; graphene_ray_intersect_sphere ()
;;; ----------------------------------------------------------------------------

(defun ray-intersect-sphere (ray sphere)
 #+liber-documentation
 "@version{2023-12-6}
  @argument[ray]{a @symbol{graphene:ray-t} instance}
  @argument[sphere]{a @symbol{graphene:sphere-t} instance}
  @begin{return}
    @arg{kind} -- a @symbol{graphene:ray-intersection-kind-t} value @br{}
    @arg{dist} -- a float with the distance of the point on the ray that
    intersects the sphere
  @end{return}
  @short{Intersects the given ray with the given sphere.}
  @see-symbol{graphene:ray-t}
  @see-symbol{graphene:sphere-t}
  @see-symbol{graphene:ray-intersection-kind-t}"
  (cffi:with-foreign-object (result :float)
    (let ((kind (cffi:foreign-funcall "graphene_ray_intersect_sphere"
                                      (:pointer (:struct ray-t)) ray
                                      (:pointer (:struct sphere-t)) sphere
                                      (:pointer :float) result
                                      ray-intersection-kind-t)))
      (values kind (cffi:mem-ref result :float)))))

(export 'ray-intersect-sphere)

;;; ----------------------------------------------------------------------------
;;; graphene_ray_intersects_sphere ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_ray_intersects_sphere" ray-intersects-sphere) :bool
 #+liber-documentation
 "@version{2023-12-6}
  @argument[ray]{a @symbol{graphene:ray-t} instance}
  @argument[sphere]{a @symbol{graphene:sphere-t} instance}
  @return{@em{True} if the ray intersects the sphere.}
  @short{Checks if the given ray intersects the given sphere.}
  @see-symbol{graphene:ray-t}
  @see-symbol{graphene:sphere-t}"
  (ray (:pointer (:struct ray-t)))
  (sphere (:pointer (:struct sphere-t))))

(export 'ray-intersects-sphere)

;;; ----------------------------------------------------------------------------
;;; graphene_ray_intersect_box ()
;;; ----------------------------------------------------------------------------

(defun ray-intersect-box (ray box)
 #+liber-documentation
 "@version{#2023-12-6}
  @argument[ray]{a @symbol{graphene:ray-t} instance}
  @argument[box]{a @symbol{graphene:box-t} instance}
  @begin{return}
    @arg{kind} -- a @symbol{graphene:ray-intersection-kind-t} value @br{}
    @arg{dist} -- a float with the distance of the point on the ray that
    intersects the box
  @end{return}
  @short{Intersects the given ray with the given box.}
  @see-symbol{graphene:ray-t}
  @see-symbol{graphene:box-t}
  @see-symbol{graphene:ray-intersection-kind-t}"
  (cffi:with-foreign-object (result :float)
    (let ((kind (cffi:foreign-funcall "graphene_ray_intersect_box"
                                      (:pointer (:struct ray-t)) ray
                                      (:pointer (:struct box-t)) box
                                      (:pointer :float) result
                                      ray-intersection-kind-t)))
      (values kind (cffi:mem-ref result :float)))))

(export 'ray-intersect-box)

;;; ----------------------------------------------------------------------------
;;; graphene_ray_intersects_box ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_ray_intersects_box" ray-intersects-box) :bool
 #+liber-documentation
 "@version{#2023-12-6}
  @argument[ray]{a @symbol{graphene:ray-t} instance}
  @argument[box]{a @symbol{graphene:box-t} instance}
  @return{@em{True} if the ray intersects the box.}
  @short{Checks if the given ray intersects the given box.}
  @see-symbol{graphene:ray-t}
  @see-symbol{graphene:box-t}"
  (ray (:pointer (:struct ray-t)))
  (box (:pointer (:struct box-t))))

(export 'ray-intersects-box)

;;; ----------------------------------------------------------------------------
;;; graphene_ray_intersect_triangle ()
;;; ----------------------------------------------------------------------------

(defun ray-intersect-triangle (ray triangle)
 #+liber-documentation
 "@version{#2023-12-6}
  @argument[ray]{a @symbol{graphene:ray-t} instance}
  @argument[box]{a @symbol{graphene:triangle-t} instance}
  @begin{return}
    @arg{kind} -- a @symbol{graphene:ray-intersection-kind-t} value @br{}
    @arg{dist} -- a float with the distance of the point on the ray that
    intersects the triangle
  @end{return}
  @short{Intersects the given ray with the given triangle.}
  @see-symbol{graphene:ray-t}
  @see-symbol{graphene:triangle-t}
  @see-symbol{graphene:ray-intersection-kind-t}"
  (cffi:with-foreign-object (result :float)
    (let ((kind (cffi:foreign-funcall "graphene_ray_intersect_triangle"
                                      (:pointer (:struct ray-t)) ray
                                      (:pointer (:struct triangle-t)) triangle
                                      (:pointer :float) result
                                      ray-intersection-kind-t)))
      (values kind (cffi:mem-ref result :float)))))

(export 'ray-intersect-triangle)

;;; ----------------------------------------------------------------------------
;;; graphene_ray_intersects_triangle ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_ray_intersects_triangle" ray-intersects-triangle) :bool
 #+liber-documentation
 "@version{#2023-12-6}
  @argument[ray]{a @symbol{graphene:ray-t} instance}
  @argument[box]{a @symbol{graphene:triangle-t} instance}
  @return{@em{True} if the ray intersects the triangle.}
  @short{Checks if the given ray intersects the given triangle.}
  @see-symbol{graphene:ray-t}
  @see-symbol{graphene:triangle-t}"
  (ray (:pointer (:struct ray-t)))
  (triangle (:pointer (:struct triangle-t))))

(export 'ray-intersects-triangle)

;;; --- End of file graphene.ray.lisp ------------------------------------------
