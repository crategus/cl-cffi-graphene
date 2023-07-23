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

(defmacro with-graphene-ray-old ((var &rest args) &body body)
  (if args
      (if (second args)
          ;; We have a list with more than one argument, use point-init
          (progn
            `(let ((,var (point-alloc)))
               (setf ,var (point-init ,var ,@args))
               (unwind-protect
                 (progn ,@body)
                 (point-free ,var))))

            (destructuring-bind (arg &optional type)
                (if (listp (first args)) (first args) (list (first args)))
              (cond ((eq type 'vec2-t)
                     ;; We have an argument of type vec2-t
                     `(let ((,var (point-alloc)))
                        (setf ,var (point-init-from-vec2 ,var ,arg))
                        (unwind-protect
                          (progn ,@body)
                          (point-free ,var))))
                    (t
                     ;; The default is an argument of type point-t
                     `(let ((,var (point-alloc)))
                        (setf ,var (point-init-from-point ,var ,arg))
                        (unwind-protect
                          (progn ,@body)
                          (point-free ,var)))))))

      (progn
        ;; The default is no initialization
        `(let ((,var (ray-alloc)))
           (unwind-protect
             (progn ,@body)
             (point-free ,var))))))

(defmacro with-graphene-ray ((var &rest args) &body body)
  (cond ((not args)
         ;; We have no arguments, the default is no initilization
         `(let ((,var (ray-alloc)))
            (unwind-protect
              (progn ,@body)
              (ray-free ,var))))
        ((not (second args))
         ;; We have one argument. The argument must be of type ray-t.
         (destructuring-bind (arg &optional type)
             (if (listp (first args)) (first args) (list (first args)))
           (cond ((or (not type)
                      (eq type 'ray-t))
                  ;; One argument with no type or of type ray-t
                  `(let ((,var (ray-alloc)))
                     (ray-init-from-ray ,var ,arg)
                     (unwind-protect
                       (progn ,@body)
                       (ray-free ,var))))
                 (t
                  (error "Type error in WITH-GRAPHENE-RAY")))))
        ((not (third args))
         ;; We have two arguments. The first can be of type point3d-t or
         ;; vec3-t. The second argument must be of type vec3-t
         ;; TODO: Combine the two destructuring-bind calls to onw call.
         (destructuring-bind (arg1 &optional type1)
             (if (listp (first args)) (first args) (list (first args)))
           (destructuring-bind (arg2 &optional type2)
               (if (listp (second args)) (second args) (list (second args)))
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
                    (error "Type error in WITH-GRAPHENE-RAY"))))))
        (t
         (error "Error in WITH-GRAPHENE-RAY"))))

(export 'with-graphene-ray)

(defmacro with-graphene-rays (vars &body body)
  (if vars
      (let ((var (if (listp (first vars)) (first vars) (list (first vars)))))
        `(with-graphene-ray ,var
           (with-graphene-rays ,(rest vars)
             ,@body)))
      `(progn ,@body)))

(export 'with-graphene-rays)

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
 "@version{#2022-9-20}
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
  @see-symbol{ray-t}")

(export 'ray-intersection-kind-t)

;;; ----------------------------------------------------------------------------
;;; graphene_ray_t
;;; ----------------------------------------------------------------------------

(cffi:defcstruct ray-t)

#+liber-documentation
(setf (liber:alias-for-symbol 'ray-t)
      "CStruct"
      (liber:symbol-documentation 'ray-t)
 "@version{#2022-9-20}
  @begin{short}
    The @sym{ray-t} structure is a structure representing a ray emitted by an
    origin, identified by a point in 3D space, in a given direction, identified
    by a vector with 3 components.
  @end{short}

  A common use of the @sym{ray-t} implementation is ray-casting to find which
  objects in a 3D scene are under the coordinates of the pointer.")

(export 'ray-t)

;;; ----------------------------------------------------------------------------
;;;graphene_ray_alloc ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_ray_alloc" ray-alloc)
    (:pointer (:struct ray-t))
 #+liber-documentation
 "@version{#2022-9-20}
  @return{The newly allocated @symbol{ray-t} instance. Use the
    @fun{ray-free} function to free the resources allocated by this function.}
  @begin{short}
    Allocates a new @symbol{ray-t} instance.
  @end{short}
  The contents of the returned structure are undefined.
  @see-symbol{ray-t}
  @see-function{ray-free}")

(export 'ray-alloc)

;;; ----------------------------------------------------------------------------
;;;graphene_ray_free ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_ray_free" ray-free) :void
 #+liber-documentation
 "@version{#2022-9-20}
  @argument[ray]{a @symbol{ray-t} instance}
  @begin{short}
    Frees the resources allocated by the @fun{ray-alloc} function.
  @end{short}
  @see-symbol{ray-t}
  @see-function{ray-alloc}"
  (ray (:pointer (:struct point-t))))

(export 'ray-free)

;;; ----------------------------------------------------------------------------
;;;graphene_ray_init ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_ray_init" ray-init) (:pointer (:struct ray-t))
 #+liber-documentation
 "@version{#2022-9-20}
  @argument[ray]{a @symbol{rayt-t} instance to be initialized}
  @argument[origin]{a @symbol{point3d-t} instance with the origin of the ray}
  @argument[direction]{a @symbol{vec3-t} instance with the direction vector}
  @return{The initialized @symbol{ray-t} instance.}
  @begin{short}
    Initializes the given @symbol{ray-t} instance using the given origin and
    direction values.
  @end{short}
  The direction vector is normalized.
  @see-symbol{ray-t}
  @see-symbol{point3d-t}
  @see-symbol{vec3-t}"
  (ray (:pointer (:struct ray-t)))
  (origin (:pointer (:struct point3d-t)))
  (direction (:pointer (:struct vec3-t))))

(export 'ray-init)

;;; ----------------------------------------------------------------------------
;;;graphene_ray_init_from_ray ()
;;;graphene_ray_t *
;;;graphene_ray_init_from_ray (graphene_ray_t *r,
;;;                            const graphene_ray_t *src);
;;;Initializes the given graphene_ray_t using the origin and direction values of another graphene_ray_t.

;;;Parameters
;;;r

;;;the graphene_ray_t to initialize

;;;src

;;;a graphene_ray_t

;;;Returns
;;;the initialized ray.

;;;Since: 1.4
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_ray_init_from_ray" ray-init-from-ray)
    (:pointer (:struct ray-t))
  (ray (:pointer (:struct ray-t)))
  (source (:pointer (:struct ray-t))))

(export 'ray-init-from-ray)

;;; ----------------------------------------------------------------------------
;;;graphene_ray_init_from_vec3 ()
;;;graphene_ray_t *
;;;graphene_ray_init_from_vec3 (graphene_ray_t *r,
;;;                             const graphene_vec3_t *origin,
;;;                             const graphene_vec3_t *direction);
;;;Initializes the given graphene_ray_t using the given vectors.

;;;Parameters
;;;r

;;;the graphene_ray_t to initialize

;;;origin

;;;a graphene_vec3_t.

;;;direction

;;;a graphene_vec3_t.

;;;Returns
;;;the initialized ray.

;;;Since: 1.4
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_ray_init_from_vec3" ray-init-from-vec3)
    (:pointer (:struct ray-t))
  (ray (:pointer (:struct ray-t)))
  (origin (:pointer (:struct vec3-t)))
  (direction (:pointer (:struct vec3-t))))

(export 'ray-init-from-vec3)

;;; ----------------------------------------------------------------------------
;;;graphene_ray_get_origin ()
;;;void
;;;graphene_ray_get_origin (const graphene_ray_t *r,
;;;                         graphene_point3d_t *origin);
;;;Retrieves the origin of the given graphene_ray_t.

;;;Parameters
;;;r

;;;a graphene_ray_t

;;;origin

;;;return location for the origin.

;;;Since: 1.4
;;; ----------------------------------------------------------------------------

(defun ray-origin (ray origin)
  (cffi:foreign-funcall "graphene_ray_get_origin"
                        (:pointer (:struct ray-t)) ray
                        (:pointer (:struct point3d-t)) origin
                        :void)
  origin)

(export 'ray-origin)

;;; ----------------------------------------------------------------------------
;;;graphene_ray_get_direction ()
;;;void
;;;graphene_ray_get_direction (const graphene_ray_t *r,
;;;                            graphene_vec3_t *direction);
;;;Retrieves the direction of the given graphene_ray_t.

;;;Parameters
;;;r

;;;a graphene_ray_t

;;;direction

;;;return location for the direction.

;;;Since: 1.4
;;; ----------------------------------------------------------------------------

(defun ray-direction (ray direction)
  (cffi:foreign-funcall "graphene_ray_get_direction"
                        (:pointer (:struct ray-t)) ray
                        (:pointer (:struct vec3-t)) direction
                        :void)
  direction)

(export 'ray-direction)

;;; ----------------------------------------------------------------------------
;;;graphene_ray_get_position_at ()
;;;void
;;;graphene_ray_get_position_at (const graphene_ray_t *r,
;;;                              float t,
;;;                              graphene_point3d_t *position);
;;;Retrieves the coordinates of a point at the distance t along the given graphene_ray_t.

;;;Parameters
;;;r

;;;a graphene_ray_t

;;;t

;;;the distance along the ray

;;;position

;;;return location for the position.

;;;Since: 1.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;graphene_ray_get_distance_to_point ()
;;;float
;;;graphene_ray_get_distance_to_point (const graphene_ray_t *r,
;;;                                    const graphene_point3d_t *p);
;;;Computes the distance of the closest approach between the given graphene_ray_t r and the point p .

;;;The closest approach to a ray from a point is the distance between the point and the projection of the point on the ray itself.

;;;Parameters
;;;r

;;;a graphene_ray_t

;;;p

;;;a graphene_point3d_t

;;;Returns
;;;the distance of the point

;;;Since: 1.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;graphene_ray_get_distance_to_plane ()
;;;float
;;;graphene_ray_get_distance_to_plane (const graphene_ray_t *r,
;;;                                    const graphene_plane_t *p);
;;;Computes the distance of the origin of the given graphene_ray_t from the given plane.

;;;If the ray does not intersect the plane, this function returns INFINITY.

;;;Parameters
;;;r

;;;a graphene_ray_t

;;;p

;;;a graphene_plane_t

;;;Returns
;;;the distance of the origin of the ray from the plane

;;;Since: 1.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;graphene_ray_get_closest_point_to_point ()
;;;void
;;;graphene_ray_get_closest_point_to_point
;;;                               (const graphene_ray_t *r,
;;;                                const graphene_point3d_t *p,
;;;                                graphene_point3d_t *res);
;;;Computes the point on the given graphene_ray_t that is closest to the given point p .

;;;Parameters
;;;r

;;;a graphene_ray_t

;;;p

;;;a graphene_point3d_t

;;;res

;;;return location for the closest point3d.

;;;Since: 1.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;graphene_ray_equal ()
;;;bool
;;;graphene_ray_equal (const graphene_ray_t *a,
;;;                    const graphene_ray_t *b);
;;;Checks whether the two given graphene_ray_t are equal.

;;;Parameters
;;;a

;;;a graphene_ray_t

;;;b

;;;a graphene_ray_t

;;;Returns
;;;true if the given rays are equal

;;;Since: 1.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;graphene_ray_intersect_sphere ()
;;;graphene_ray_intersection_kind_t
;;;graphene_ray_intersect_sphere (const graphene_ray_t *r,
;;;                               const graphene_sphere_t *s,
;;;                               float *t_out);
;;;Intersects the given graphene_ray_t r with the given graphene_sphere_t s .

;;;Parameters
;;;r

;;;a graphene_ray_t

;;;s

;;;a graphene_sphere_t

;;;t_out

;;;the distance of the point on the ray that intersects the sphere.

;;;Returns
;;;the type of intersection

;;;Since: 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;graphene_ray_intersects_sphere ()
;;;bool
;;;graphene_ray_intersects_sphere (const graphene_ray_t *r,
;;;                                const graphene_sphere_t *s);
;;;Checks if the given graphene_ray_t r intersects the given graphene_sphere_t s .

;;;See also: graphene_ray_intersect_sphere()

;;;Parameters
;;;r

;;;a graphene_ray_t

;;;s

;;;a graphene_sphere_t

;;;Returns
;;;true if the ray intersects the sphere

;;;Since: 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;graphene_ray_intersect_box ()
;;;graphene_ray_intersection_kind_t
;;;graphene_ray_intersect_box (const graphene_ray_t *r,
;;;                            const graphene_box_t *b,
;;;                            float *t_out);
;;;Intersects the given graphene_ray_t r with the given graphene_box_t b .

;;;Parameters
;;;r

;;;a graphene_ray_t

;;;b

;;;a graphene_box_t

;;;t_out

;;;the distance of the point on the ray that intersects the box.

;;;Returns
;;;the type of intersection

;;;Since: 1.10

;;;graphene_ray_intersects_box ()
;;;bool
;;;graphene_ray_intersects_box (const graphene_ray_t *r,
;;;                             const graphene_box_t *b);
;;;Checks whether the given graphene_ray_t r intersects the given graphene_box_t b .

;;;See also: graphene_ray_intersect_box()

;;;Parameters
;;;r

;;;a graphene_ray_t

;;;b

;;;a graphene_box_t

;;;Returns
;;;true if the ray intersects the box

;;;Since: 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;graphene_ray_intersect_triangle ()
;;;graphene_ray_intersection_kind_t
;;;graphene_ray_intersect_triangle (const graphene_ray_t *r,
;;;                                 const graphene_triangle_t *t,
;;;                                 float *t_out);
;;;Intersects the given graphene_ray_t r with the given graphene_triangle_t t .

;;;Parameters
;;;r

;;;a graphene_ray_t

;;;t

;;;a graphene_triangle_t

;;;t_out

;;;the distance of the point on the ray that intersects the triangle.

;;;Returns
;;;the type of intersection

;;;Since: 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;graphene_ray_intersects_triangle ()
;;;bool
;;;graphene_ray_intersects_triangle (const graphene_ray_t *r,
;;;                                  const graphene_triangle_t *t);
;;;Checks whether the given graphene_ray_t r intersects the given graphene_triangle_t b .

;;;See also: graphene_ray_intersect_triangle()

;;;Parameters
;;;r

;;;a graphene_ray_t

;;;t

;;;a graphene_triangle_t

;;;Returns
;;;true if the ray intersects the triangle

;;;Since: 1.10
;;; ----------------------------------------------------------------------------

;;; --- End of file graphene.ray.lisp ------------------------------------------
