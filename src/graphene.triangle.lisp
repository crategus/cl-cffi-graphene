;;; ----------------------------------------------------------------------------
;;; graphene.triangle.lisp
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
;;; Triangle
;;;
;;;     A triangle described by 3D points
;;;
;;; Types and Values
;;;
;;;     graphene_triangle_t
;;;
;;; Functions
;;;
;;;     graphene_triangle_alloc
;;;     graphene_triangle_free
;;;     graphene_triangle_init_from_point3d
;;;     graphene_triangle_init_from_vec3
;;;     graphene_triangle_init_from_float
;;;     graphene_triangle_get_points
;;;     graphene_triangle_get_vertices
;;;     graphene_triangle_get_area
;;;     graphene_triangle_get_midpoint
;;;     graphene_triangle_get_normal
;;;     graphene_triangle_get_plane
;;;     graphene_triangle_get_bounding_box
;;;     graphene_triangle_get_barycoords
;;;     graphene_triangle_get_uv
;;;     graphene_triangle_contains_point
;;;     graphene_triangle_equal
;;; ----------------------------------------------------------------------------

(in-package :graphene)

(defmacro with-triangle ((var &rest args) &body body)
 #+liber-documentation
 "@version{2023-12-9}
  @syntax[]{(graphene:with-triangle (triangle) body) => result}
  @syntax[]{(graphene:with-triangle (triangle p1 p2 p3) body) => result}
  @syntax[]{(graphene:with-triangle (triangle v1 v2 v3) body) => result}
  @argument[triangle]{a @symbol{graphene:triangle-t} instance to create and
    initialize}
  @argument[p1]{a @symbol{graphene:point3d-t} instance to use for
    initialization}
  @argument[p2]{a @symbol{graphene:point3d-t} instance to use for
    initialization}
  @argument[p3]{a @symbol{graphene:point3d-t} instance to use for
    initialization}
  @argument[v1]{a @symbol{graphene:vec3-t} instance to use for initialization}
  @argument[v2]{a @symbol{graphene:vec3-t} instance to use for initialization}
  @argument[v3]{a @symbol{graphene:vec3-t} instance to use for initialization}
  @begin{short}
    The @fun{graphene:with-triangle} macro allocates a new
    @symbol{graphene:triangle-t} instance, initializes the triangle with the
    given values and executes the body that uses the box.
  @end{short}
  After execution of the body the allocated memory for the triangle is released.

  When no argument is given the components of the triangle are not definied.
  The initialization with three points uses the
  @fun{graphene:triangle-init-from-point3d} function. If the first value has the
  @code{graphene:vec3-t} type the @fun{graphene:triangle-init-from-vec3} is used
  for initialization with threee vectors.
  @begin[Note]{dictionary}
    The memory is allocated with the @fun{graphene:triangle-alloc} function and
    released with the @fun{graphene:triangle-free} function.
  @end{dictionary}
  @see-symbol{graphene:triangle-t}
  @see-symbol{graphene:point3d-t}
  @see-symbol{grapene:vec3-t}
  @see-macro{graphene:with-triangles}
  @see-function{graphene:triangle-alloc}
  @see-function{graphene:triangle-free}"
  (cond ((null args)
         ;; No arguments, the default is no initialization
         `(let ((,var (triangle-alloc)))
            (unwind-protect
              (progn ,@body)
              (triangle-free ,var))))
        ((null (fourth args))
         ;; Three arguments, the first can be of type point3d-t or vec3-t.
         (destructuring-bind ((arg1 &optional type1)
                              (arg2 &optional type2)
                              (arg3 &optional type3))
             (list (mklist (first args))
                   (mklist (second args))
                   (mklist (third args)))
           (cond ((and (or (not type1)
                           (eq type1 'point3d-t))
                       (or (not type2)
                           (eq type2 'point3d-t))
                       (or (not type3)
                           (eq type3 'point3d-t)))
                  ;; First argument with no type or of type point3d-t and
                  ;; second and third argument with no type or type point3d-t
                  `(let ((,var (triangle-alloc)))
                     (triangle-init-from-point3d ,var ,arg1 ,arg2 ,arg3)
                     (unwind-protect
                       (progn ,@body)
                       (triangle-free ,var))))
                 ((and (eq type1 'vec3-t)
                       (or (not type2)
                           (eq type2 'vec3-t))
                       (or (not type3)
                           (eq type3 'vec3-t)))
                  ;; First argument of type vec3-t and second and third argument
                  ;; with no type or type vec3-t
                  `(let ((,var (triangle-alloc)))
                     (triangle-init-from-vec3 ,var ,arg1 ,arg2 ,arg3)
                     (unwind-protect
                       (progn ,@body)
                       (triangle-free ,var))))
                 (t
                  (error "Syntax error in GRAPHENE:WITH-TRIANGLE")))))
        (t
         (error "Syntax error in GRAPHENE:WITH-TRIANGLE"))))

(export 'with-triangle)

(defmacro with-triangles (vars &body body)
 #+liber-documentation
 "@version{2023-12-9}
  @syntax[]{(graphene:with-triangles (triangle1 ... trianglen) body) => result}
  @argument[triangle1 ... trianglen]{the newly created
    @symbol{graphene:triangle-t} instances}
  @argument[body]{a body that uses the bindings @arg{triangle1 ... trianglen}}
  @begin{short}
    The @fun{graphene:with-triangles} macro creates new variable bindings and
    executes the body that use these bindings.
  @end{short}
  The macro performs the bindings sequentially, like the @sym{let*} macro.

  Each point can be initialized with values using the syntax for the
  @fun{graphene:with-triangle} macro. See also the @fun{graphene:with-triangle}
  documentation.
  @see-symbol{graphene:triangle-t}
  @see-macro{graphene:with-triangle}"
  (if vars
      (let ((var (mklist (first vars))))
        `(with-triangle ,var
           (with-triangles ,(rest vars)
             ,@body)))
      `(progn ,@body)))

(export 'with-triangles)

;;; ----------------------------------------------------------------------------
;;; graphene_triangle_t
;;; ----------------------------------------------------------------------------

(cffi:defcstruct triangle-t)

#+liber-documentation
(setf (liber:alias-for-symbol 'triangle-t)
      "CStruct"
      (liber:symbol-documentation 'triangle-t)
 "@version{#2023-12-7}
  @begin{short}
    The @symbol{graphene:triangle-t} structure represents a triangle in 3D
    space.
  @end{short}")

(export 'triangle-t)

;;; ----------------------------------------------------------------------------
;;; graphene_triangle_alloc ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_triangle_alloc" triangle-alloc)
    (:pointer (:struct triangle-t))
 #+liber-documentation
 "@version{#2023-12-7}
  @return{The newly allocated @symbol{graphene:triangle-t} instance.}
  @begin{short}
    Allocates a new @symbol{graphene:triangle-t} instance.
  @end{short}
  The contents of the returned instance are undefined. Use the
  @fun{graphene:triangle-free} function to free the resources allocated by this
  function.
  @see-symbol{graphene:triangle-t}
  @see-function{graphene:triangle-free}")

(export 'triangle-alloc)

;;; ----------------------------------------------------------------------------
;;; graphene_triangle_free ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_triangle_free" triangle-free) :void
 #+liber-documentation
 "@version{#2023-12-7}
  @argument[triangle]{a @symbol{graphene:triangle-t} instance}
  @begin{short}
    Frees the resources allocated by the @fun{graphene:triangle-alloc} function.
  @end{short}
  @see-symbol{graphene:triangle-t}
  @see-function{graphene:triangle-alloc}"
  (triangle (:pointer (:struct triangle-t))))

(export 'triangle-free)

;;; ----------------------------------------------------------------------------
;;; graphene_triangle_init_from_point3d ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_triangle_init_from_point3d" triangle-init-from-point3d)
    (:pointer (:struct triangle-t))
 #+liber-documentation
 "@version{#2023-12-7}
  @argument[triangle]{a @symbol{graphene:triangle-t} instance}
  @argument[a]{a @symbol{graphene:point3d-t} instance}
  @argument[b]{a @symbol{graphene:point3d-t} instance}
  @argument[c]{a @symbol{graphene:point3d-t} instance}
  @return{The initialized @symbol{graphene:triangle-t} instance.}
  @short{Initializes a triangle using the three given 3D points.}
  @see-symbol{graphene:triangle-t}
  @see-symbol{graphene:point3d-t}"
  (triangle (:pointer (:struct triangle-t)))
  (a (:pointer (:struct point3d-t)))
  (b (:pointer (:struct point3d-t)))
  (c (:pointer (:struct point3d-t))))

(export 'triangle-init-from-point3d)

;;; ----------------------------------------------------------------------------
;;; graphene_triangle_init_from_vec3 ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_triangle_init_from_vec3" triangle-init-from-vec3)
    (:pointer (:struct triangle-t))
 #+liber-documentation
 "@version{#2023-12-7}
  @argument[triangle]{a @symbol{graphene:triangle-t} instance}
  @argument[a]{a @symbol{graphene:vec3-t} instance}
  @argument[b]{a @symbol{graphene:vec3-t} instance}
  @argument[c]{a @symbol{graphene:vec3-t} instance}
  @return{The initialized @symbol{graphene:triangle-t} instance.}
  @short{Initializes a triangle using the three given vectors.}
  @see-symbol{graphene:sphere-t}
  @see-symbol{graphene:vec3-t}"
  (triangle (:pointer (:struct vec3-t)))
  (a (:pointer (:struct vec3-t)))
  (b (:pointer (:struct vec3-t)))
  (c (:pointer (:struct vec3-t))))

(export 'triangle-init-from-vec3)

;;; ----------------------------------------------------------------------------
;;; graphene_triangle_init_from_float ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_triangle_init_from_float" %triangle-init-from-float)
    (:pointer (:struct triangle-t))
  (triangle (:pointer (:struct triangle-t)))
  (aptr (:pointer :float))
  (bptr (:pointer :float))
  (cptr (:pointer :float)))

(defun triangle-init-from-float (triangle a b c)
 #+liber-documentation
 "@version{#2023-12-7}
  @argument[triangle]{a @symbol{graphene:triangle-t} instance}
  @argument[a]{a list of 3 floating point values}
  @argument[b]{a list of 3 floating point values}
  @argument[c]{a list of 3 floating point values}
  @return{The initialized @symbol{graphene:triangle-t} instance.}
  @begin{short}
    Initializes a triangle using the three given list of floating point values,
    each representing the coordinates of a point in 3D space.
  @end{short}
  @see-symbol{graphene:triangle-t}"
  (cffi:with-foreign-objects ((aptr :float 3) (bptr :float 3) (cptr :float 3))
    (iter (for i from 0 below 3)
          (for av in a)
          (for bv in b)
          (for cv in c)
          (setf (cffi:mem-aref aptr :float i) (coerce av 'single-float)
                (cffi:mem-aref bptr :float i) (coerce bv 'single-float)
                (cffi:mem-aref cptr :float i) (coerce cv 'single-float)))
    (%triangle-init-from-float triangle aptr bptr cptr)))

(export 'triangle-init-from-float)

;;; ----------------------------------------------------------------------------
;;; graphene_triangle_get_points ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_triangle_get_points" %triangle-points) :void
  (triangle (:pointer (:struct triangle-t)))
  (a (:pointer (:struct point3d-t)))
  (b (:pointer (:struct point3d-t)))
  (c (:pointer (:struct point3d-t))))

(defun triangle-points (triangle a b c)
 #+liber-documentation
 "@version{#2023-12-7}
  @argument[triangle]{a @symbol{graphene:triangle-t} instance}
  @argument[a]{a @symbol{graphene:point3d-t} instance for the first vertex}
  @argument[b]{a @symbol{graphene:point3d-t} instance for the second vertex}
  @argument[c]{a @symbol{graphene:point3d-t} instance for the third vertex}
  @return{The value list of @symbol{graphene:point3d-t} instances with the
    coordinates of the vertices.}
  @begin{short}
    Retrieves the three vertices of the given triangle and returns their
    coordinates as @symbol{graphene:point3d-t} instances.
  @end{short}
  @see-symbol{graphene:triangle-t}
  @see-symbol{graphene:point3d-t}"
  (%triangle-points triangle a b c)
  (values a b c))

(export 'triangle-points)

;;; ----------------------------------------------------------------------------
;;; graphene_triangle_get_vertices ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_triangle_get_vertices" %triangle-vertices) :void
  (triangle (:pointer (:struct triangle-t)))
  (a (:pointer (:struct vec3-t)))
  (b (:pointer (:struct vec3-t)))
  (c (:pointer (:struct vec3-t))))

(defun triangle-vertices (triangle a b c)
 #+liber-documentation
 "@version{#2023-12-7}
  @argument[triangle]{a @symbol{graphene:triangle-t} instance}
  @argument[a]{a @symbol{graphene:vec3-t} instance for the first vertex}
  @argument[b]{a @symbol{graphene:vec3-t} instance for the second vertex}
  @argument[c]{a @symbol{graphene:vec3-t} instance for the third vertex}
  @return{The value list of @symbol{graphene:vec3-t} instances with the
    vertices.}
  @short{Retrieves the three vertices of the given triangle.}
  @see-symbol{graphene:triangle-t}
  @see-symbol{graphene:vec3-t}"
  (%triangle-vertices triangle a b c)
  (values a b c))

(export 'triangle-vertices)

;;; ----------------------------------------------------------------------------
;;; graphene_triangle_get_area ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_triangle_get_area" triangle-area) :float
 #+liber-documentation
 "@version{#2023-12-7}
  @argument[triangle]{a @symbol{graphene:triangle-t} instance}
  @return{The float with the area of the triangle.}
  @short{Computes the area of the given triangle.}
  @see-symbol{graphene:triangle-t}"
  (triangle (:pointer (:struct triangle-t))))

(export 'triangle-area)

;;; ----------------------------------------------------------------------------
;;; graphene_triangle_get_midpoint ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_triangle_get_midpoint" %triangle-midpoint) :void
  (triangle (:pointer (:struct triangle-t)))
  (result (:pointer (:struct point3d-t))))

(defun triangle-midpoint (triangle result)
 #+liber-documentation
 "@version{#2023-12-8}
  @argument[triangle]{a @symbol{graphene:triangle-t} instance}
  @argument[result]{a @symbol{graphene:point3d-t} instance for the coordinates
    of the midpoint}
  @return{The @symbol{graphene:point3d-t} instance with the coordinates of
    the midpoint.}
  @begin{short}
    Computes the coordinates of the midpoint of the given triangle.
  @end{short}
  The midpoint is the centroid of the triangle, i.e. the intersection of its
  medians.
  @see-symbol{graphene:triangle-t}
  @see-symbol{graphene:point3d-t}"
  (%triangle-midpoint triangle result)
  result)

(export 'triangle-midpoint)

;;; ----------------------------------------------------------------------------
;;; graphene_triangle_get_normal ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_triangle_get_normal" %triangle-normal) :void
  (triangle (:pointer (:struct triangle-t)))
  (result (:pointer (:struct vec3-t))))

(defun triangle-normal (triangle result)
 #+liber-documentation
 "@version{#2023-12-8}
  @argument[triangle]{a @symbol{graphene:triangle-t} instance}
  @argument[result]{a @symbol{graphene:vec3-t} instance for the normal vector}
  @return{The @symbol{graphene:vec3-t} instance with the normal vector.}
  @short{Computes the normal vector of the given triangle.}
  @see-symbol{graphene:triangle-t}
  @see-symbol{graphene:vec3-t}"
  (%triangle-normal triangle result)
  result)

(export 'triangle-normal)

;;; ----------------------------------------------------------------------------
;;; graphene_triangle_get_plane ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_triangle_get_plane" %triangle-plane) :void
  (triangle (:pointer (:struct triangle-t)))
  (result (:pointer (:struct plane-t))))

(defun triangle-plane (triangle result)
 #+liber-documentation
 "@version{#2023-12-8}
  @argument[triangle]{a @symbol{graphene:triangle-t} instance}
  @argument[result]{a @symbol{graphene:plane-t} instance for the plane}
  @return{The @symbol{graphene:plane-t} instance with the plane.}
  @short{Computes the plane based on the vertices of the given triangle.}
  @see-symbol{graphene:triangle-t}
  @see-symbol{graphene:plane-t}"
  (%triangle-plane triangle result)
  result)

(export 'triangle-plane)

;;; ----------------------------------------------------------------------------
;;; graphene_triangle_get_bounding_box ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_triangle_get_bounding_box" %triangle-bounding-box)
    :void
  (triangle (:pointer (:struct triangle-t)))
  (result (:pointer (:struct box-t))))

(defun triangle-bounding-box (triangle result)
 #+liber-documentation
 "@version{#2023-12-8}
  @argument[triangle]{a @symbol{graphene:triangle-t} instance}
  @argument[result]{a @symbol{graphene:box-t} instance for the box}
  @return{The @symbol{graphene:box-t} instance with the box.}
  @short{Computes the bounding box of the given triangle.}
  @see-symbol{graphene:triangle-t}
  @see-symbol{graphene:box-t}"
  (%triangle-bounding-box triangle result)
  result)

(export 'triangle-bounding-box)

;;; ----------------------------------------------------------------------------
;;; graphene_triangle_get_barycoords ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_triangle_get_barycoords" %triangle-barycoords) :bool
  (triangle (:pointer (:struct triangle-t)))
  (point (:pointer (:struct point3d-t)))
  (result (:pointer (:struct vec2-t))))

(defun triangle-barycoords (triangle point result)
 #+liber-documentation
 "@version{#2023-12-8}
  @argument[triangle]{a @symbol{graphene:triangle-t} instance}
  @argument[point]{a @symbol{graphene:point3d-t} instance}
  @argument[result]{a @symbol{graphene:vec2-t} instance for the vector with the
    barycentric coordinates}
  @return{The @symbol{graphene:vec2-t} instance with the barycentric
    coordinates, or @code{nil}  if the result is not valid}
  @begin{short}
    Computes the barycentric coordinates of the given point.
  @end{short}
  The point must lie on the same plane as the triangle. If the point is not
  coplanar, the result of this function is undefined.

  If we place the origin in the coordinates of the triangle's A point, the
  barycentric coordinates are u, which is on the AC vector, and v which is on
  the AB vector.

  The returned vector contains the following values, in order:
  @begin{pre}
result.x = u
result.y = v
  @end{pre}
  @see-symbol{graphene:triangle-t}
  @see-symbol{graphene:point3d-t}
  @see-symbol{graphene:vec2-t}"
  (when (%triangle-barycoords triangle point result)
    result))

(export 'triangle-barycoords)

;;; ----------------------------------------------------------------------------
;;; graphene_triangle_get_uv ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_triangle_get_uv" %triangle-uv) :bool
  (triangle (:pointer (:struct triangle-t)))
  (point (:pointer (:struct point3d-t)))
  (a (:pointer (:struct vec2-t)))
  (b (:pointer (:struct vec2-t)))
  (c (:pointer (:struct vec2-t)))
  (result (:pointer (:struct vec2-t))))

(defun triangle-uv (triangle point a b c result)
 #+liber-documentation
 "@version{#2023-12-7}
  @argument[triangle]{a @symbol{graphene:triangle-t} instance}
  @argument[point]{a @symbol{graphene:point3d-t} instance}
  @argument[a]{a @symbol{graphene:vec2-t} instance with the UV coordinates
    of the first point}
  @argument[b]{a @symbol{graphene:vec2-t} instance with the UV coordinates
    of the second point}
  @argument[c]{a @symbol{graphene:vec2-t} instance with the UV coordinates
    of the third point}
  @argument[result]{a @symbol{graphene:vec2-t} instance containing the UV
    coordinates of the given point}
  @return{The @symbol{graphene:vec2-t} instance with the UV coordinates of the
    given point, of @code{nil} if the result is not valid.}
  @begin{short}
    Computes the UV coordinates of the given point.
  @end{short}
  The point must lie on the same plane as the triangle. If the point is not
  coplanar, the result of this function is undefined. If @arg{point} is
  @code{nil}, the point will be set to (0, 0, 0).

  The UV coordinates will be placed in the result vector:
  @begin{pre}
result.x = u
result.y = v
  @end{pre}
  See also the @fun{graphene:triangle-barycoords} function.
  @see-symbol{graphene:triangle-t}
  @see-symbol{graphene:point3d-t}
  @see-symbol{graphene:vec2-t}
  @see-function{graphene:triangle-barycoords}"
  (when (%triangle-uv triangle point a b c result)
    result))

(export 'triangle-uv)

;;; ----------------------------------------------------------------------------
;;; graphene_triangle_contains_point ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_triangle_contains_point" triangle-contains-point) :bool
 #+liber-documentation
 "@version{#2023-12-7}
  @argument[triangle]{a @symbol{graphene:triangle-t} instance}
  @argument[point]{a @symbol{graphene:point3d-t} instance}
  @return{@em{True} if the point is inside the triangle.}
  @short{Checks whether the given triangle contains the point.}
  @see-symbol{graphene:triangle-t}
  @see-symbol{graphene:point3d-t}"
  (triangle (:pointer (:struct triangle-t)))
  (point (:pointer (:struct point3d-t))))

(export 'triangle-contains-point)

;;; ----------------------------------------------------------------------------
;;; graphene_triangle_equal ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_triangle_equal" triangle-equal) :bool
 #+liber-documentation
 "@version{#2023-12-7}
  @argument[a]{a @symbol{graphene:triangle-t} instance}
  @argument[b]{a @symbol{graphene:triangle-t} instance}
  @return{@em{True} if the triangles are equal.}
  @short{Checks whether the two given triangles are equal.}
  @see-symbol{graphene:triangle-t}"
  (a (:pointer (:struct triangle-t)))
  (b (:pointer (:struct triangle-t))))

(export 'triangle-equal)

;;; --- End of file graphene.triangle.lisp -------------------------------------
