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

;;; ----------------------------------------------------------------------------
;;; graphene_triangle_t
;;; ----------------------------------------------------------------------------

(cffi:defcstruct triangle-t)

#+liber-documentation
(setf (liber:alias-for-symbol 'triangle-t)
      "CStruct"
      (liber:symbol-documentation 'triangle-t)
 "@version{#2023-11-21}
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
 "@version{#2023-11-21}
  @return{The newly allocated @symbol{graphene:triangle-t} instance. Use the
    @fun{graphene:triangle-free} function to free the resources allocated by
    this function.}
  @begin{short}
    Allocates a new @symbol{graphene:triangle-t} instance.
  @end{short}
  The contents of the returned instance are undefined.
  @see-symbol{graphene:triangle-t}
  @see-function{graphene:triangle-free}")

(export 'triangle-alloc)

;;; ----------------------------------------------------------------------------
;;; graphene_triangle_free ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_triangle_free" triangle-free) :void
 #+liber-documentation
 "@version{#2023-11-21}
  @argument[t]{a @symbol{graphene:triangle-t} instance}
  @begin{short}
    Frees the resources allocated by the @fun{graphene:triangle-alloc} function.
  @end{short}
  @see-symbol{graphene:triangle-t}
  @see-function{graphene:triangle-alloc}"
  (tr (:pointer (:struct triangle-t))))

(export 'triangle-free)

;;; ----------------------------------------------------------------------------
;;; graphene_triangle_init_from_point3d ()
;;;
;;; graphene_triangle_t *
;;; graphene_triangle_init_from_point3d (graphene_triangle_t *t,
;;;                                      const graphene_point3d_t *a,
;;;                                      const graphene_point3d_t *b,
;;;                                      const graphene_point3d_t *c);
;;;
;;; Initializes a graphene_triangle_t using the three given 3D points.
;;;
;;; t
;;;     the graphene_triangle_t to initialize
;;;
;;; a
;;;     a graphene_point3d_t.
;;;
;;; b
;;;     a graphene_point3d_t.
;;;
;;; c
;;;     a graphene_point3d_t.
;;;
;;; Returns
;;;     the initialized graphene_triangle_t.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gaphene_triangle_init_from_point3d" triangle-init-from-point3d)
    (:pointer (:struct triangle-t))
  (tr (:pointer (:struct triangle-t)))
  (a (:pointer (:struct point3d-t)))
  (b (:pointer (:struct point3d-t)))
  (c (:pointer (:struct point3d-t))))

(export 'triangle-init-from-point3d)

;;; ----------------------------------------------------------------------------
;;; graphene_triangle_init_from_vec3 ()
;;;
;;; graphene_triangle_t *
;;; graphene_triangle_init_from_vec3 (graphene_triangle_t *t,
;;;                                   const graphene_vec3_t *a,
;;;                                   const graphene_vec3_t *b,
;;;                                   const graphene_vec3_t *c);
;;;
;;; Initializes a graphene_triangle_t using the three given vectors.
;;;
;;; t
;;;     the graphene_triangle_t to initialize
;;;
;;; a
;;;     a graphene_vec3_t.
;;;
;;; b
;;;     a graphene_vec3_t.
;;;
;;; c
;;;     a graphene_vec3_t.
;;;
;;; Returns
;;;     the initialized graphene_triangle_t.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gaphene_triangle_init_from_vec3" triangle-init-from-vec3)
    (:pointer (:struct triangle-t))
  (tr (:pointer (:struct vec3-t)))
  (a (:pointer (:struct vec3-t)))
  (b (:pointer (:struct vec3-t)))
  (c (:pointer (:struct vec3-t))))

(export 'triangle-init-from-vec3)

;;; ----------------------------------------------------------------------------
;;; graphene_triangle_init_from_float ()
;;;
;;; graphene_triangle_t *
;;; graphene_triangle_init_from_float (graphene_triangle_t *t,
;;;                                    const float *a,
;;;                                    const float *b,
;;;                                    const float *c);
;;;
;;; Initializes a graphene_triangle_t using the three given arrays of floating
;;; point values, each representing the coordinates of a point in 3D space.
;;;
;;; t
;;;     the graphene_triangle_t to initialize
;;;
;;; a
;;;     an array of 3 floating point values.
;;;
;;; b
;;;     an array of 3 floating point values.
;;;
;;; c
;;;     an array of 3 floating point values.
;;;
;;; Returns
;;;     the initialized graphene_triangle_t.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; graphene_triangle_get_points ()
;;;
;;; void
;;; graphene_triangle_get_points (const graphene_triangle_t *t,
;;;                               graphene_point3d_t *a,
;;;                               graphene_point3d_t *b,
;;;                               graphene_point3d_t *c);
;;;
;;; Retrieves the three vertices of the given graphene_triangle_t and returns
;;; their coordinates as graphene_point3d_t.
;;;
;;; t
;;;     a graphene_triangle_t
;;;
;;; a
;;;     return location for the coordinates of the first vertex.
;;;
;;; b
;;;     return location for the coordinates of the second vertex.
;;;
;;; c
;;;     return location for the coordinates of the third vertex.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_triangle_get_points" triangle-points) :void
  (tr (:pointer (:struct triangle-t)))
  (a (:pointer (:struct point3d-t)))
  (b (:pointer (:struct point3d-t)))
  (c (:pointer (:struct point3d-t))))

(export 'triangle-points)

;;; ----------------------------------------------------------------------------
;;; graphene_triangle_get_vertices ()
;;;
;;; void
;;; graphene_triangle_get_vertices (const graphene_triangle_t *t,
;;;                                 graphene_vec3_t *a,
;;;                                 graphene_vec3_t *b,
;;;                                 graphene_vec3_t *c);
;;;
;;; Retrieves the three vertices of the given graphene_triangle_t.
;;;
;;; t
;;;     a graphene_triangle_t
;;;
;;; a
;;;     return location for the first vertex.
;;;
;;; b
;;;     return location for the second vertex.
;;;
;;; c
;;;     return location for the third vertex.
;;;
;;; Since: 1.2
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_triangle_get_vertices" triangle-vertices) :void
  (tr (:pointer (:struct triangle-t)))
  (a (:pointer (:struct vec3-t)))
  (b (:pointer (:struct vec3-t)))
  (c (:pointer (:struct vec3-t))))

(export 'triangle-vertices)

;;; ----------------------------------------------------------------------------
;;; graphene_triangle_get_area ()
;;;
;;; float
;;; graphene_triangle_get_area (const graphene_triangle_t *t);
;;;
;;; Computes the area of the given graphene_triangle_t.
;;;
;;; t
;;;     a graphene_triangle_t
;;;
;;; Returns
;;;     the area of the triangle
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_triangle_get_area" triangle-area) :float
  (tr (:pointer (:struct triangle-t))))

(export 'triangle-area)

;;; ----------------------------------------------------------------------------
;;; graphene_triangle_get_midpoint ()
;;;
;;; void
;;; graphene_triangle_get_midpoint (const graphene_triangle_t *t,
;;;                                 graphene_point3d_t *res);
;;;
;;; Computes the coordinates of the midpoint of the given graphene_triangle_t.
;;;
;;; The midpoint G is the centroid of the triangle, i.e. the intersection of
;;; its medians.
;;;
;;; t
;;;     a graphene_triangle_t
;;;
;;; res
;;;     return location for the coordinates of the midpoint.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_triangle_get_midpoint" triangle-midpoint) :void
  (tr (:pointer (:struct triangle-t)))
  (result (:pointer (:struct point3d-t))))

(export 'triangle-midpoint)

;;; ----------------------------------------------------------------------------
;;; graphene_triangle_get_normal ()
;;;
;;; void
;;; graphene_triangle_get_normal (const graphene_triangle_t *t,
;;;                               graphene_vec3_t *res);
;;;
;;; Computes the normal vector of the given graphene_triangle_t.
;;;
;;; t
;;;     a graphene_triangle_t
;;;
;;; res
;;;     return location for the normal vector.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_triangle_get_normal" triangle-normal) :void
  (tr (:pointer (:struct triangle-t)))
  (result (:pointer (:struct vec3-t))))

(export 'triangle-normal)

;;; ----------------------------------------------------------------------------
;;; graphene_triangle_get_plane ()
;;;
;;; void
;;; graphene_triangle_get_plane (const graphene_triangle_t *t,
;;;                              graphene_plane_t *res);
;;;
;;; Computes the plane based on the vertices of the given graphene_triangle_t.
;;;
;;; t
;;;     a graphene_triangle_t
;;;
;;; res
;;;     return location for the plane.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_triangle_get_plane" triangle-plane) :void
  (tr (:pointer (:struct triangle-t)))
  (result (:pointer (:struct plane-t))))

(export 'triangle-plane)

;;; ----------------------------------------------------------------------------
;;; graphene_triangle_get_bounding_box ()
;;;
;;; void
;;; graphene_triangle_get_bounding_box (const graphene_triangle_t *t,
;;;                                     graphene_box_t *res);
;;;
;;; Computes the bounding box of the given graphene_triangle_t.
;;;
;;; t
;;;     a graphene_triangle_t
;;;
;;; res
;;;     return location for the box.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_triangle_get_bounding_box" triangle-bounding-box) :void
  (tr (:pointer (:struct triangle-t)))
  (result (:pointer (:struct box-t))))

(export 'triangle-bounding-box)

;;; ----------------------------------------------------------------------------
;;; graphene_triangle_get_barycoords ()
;;;
;;; bool
;;; graphene_triangle_get_barycoords (const graphene_triangle_t *t,
;;;                                   const graphene_point3d_t *p,
;;;                                   graphene_vec2_t *res);
;;;
;;; Computes the barycentric coordinates of the given point p .
;;;
;;; The point p must lie on the same plane as the triangle t ; if the point is
;;; not coplanar, the result of this function is undefined.
;;;
;;; If we place the origin in the coordinates of the triangle's A point, the
;;; barycentric coordinates are u, which is on the AC vector; and v which is on
;;; the AB vector:
;;;
;;; The returned graphene_vec2_t contains the following values, in order:
;;;
;;; res.x = u
;;; res.y = v
;;;
;;; t
;;;     a graphene_triangle_t
;;;
;;; p
;;;     a graphene_point3d_t.
;;;
;;; res
;;;     return location for the vector with the barycentric coordinates.
;;;
;;; Returns
;;;     true if the barycentric coordinates are valid
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_triangle_get_barycoords" triangle-barycoords) :bool
  (tr (:pointer (:struct triangle-t)))
  (p (:pointer (:struct point3d-t)))
  (result (:pointer (:struct vec2-t))))

(export 'triangle-barycoords)

;;; ----------------------------------------------------------------------------
;;; graphene_triangle_get_uv ()
;;;
;;; bool
;;; graphene_triangle_get_uv (const graphene_triangle_t *t,
;;;                           const graphene_point3d_t *p,
;;;                           const graphene_vec2_t *uv_a,
;;;                           const graphene_vec2_t *uv_b,
;;;                           const graphene_vec2_t *uv_c,
;;;                           graphene_vec2_t *res);
;;;
;;; Computes the UV coordinates of the given point p .
;;;
;;; The point p must lie on the same plane as the triangle t ; if the point is
;;; not coplanar, the result of this function is undefined. If p is NULL, the
;;; point will be set in (0, 0, 0).
;;;
;;; The UV coordinates will be placed in the res vector:
;;;
;;; res.x = u
;;; res.y = v
;;;
;;; See also: graphene_triangle_get_barycoords()
;;;
;;; t
;;;     a graphene_triangle_t
;;;
;;; p
;;;     a graphene_point3d_t.
;;;
;;; uv_a
;;;     the UV coordinates of the first point
;;;
;;; uv_b
;;;     the UV coordinates of the second point
;;;
;;; uv_c
;;;     the UV coordinates of the third point
;;;
;;; res
;;;     a vector containing the UV coordinates of the given point p .
;;;
;;; Returns
;;;     true if the coordinates are valid
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_triangle_get_uv" triangle-uv) :bool
  (tr (:pointer (:struct triangle-t)))
  (p (:pointer (:struct point3d-t)))
  (a (:pointer (:struct vec2-t)))
  (b (:pointer (:struct vec2-t)))
  (c (:pointer (:struct vec2-t)))
  (result (:pointer (:struct vec2-t))))

(export 'triangle-uv)

;;; ----------------------------------------------------------------------------
;;; graphene_triangle_contains_point ()
;;;
;;; bool
;;; graphene_triangle_contains_point (const graphene_triangle_t *t,
;;;                                   const graphene_point3d_t *p);
;;;
;;; Checks whether the given triangle t contains the point p .
;;;
;;; t
;;;     a graphene_triangle_t
;;;
;;; p
;;;     a graphene_point3d_t
;;;
;;; Returns
;;;     true if the point is inside the triangle
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_triangle_contains_point" triangle-contains-point) :bool
  (tr (:pointer (:struct triangle-t)))
  (p (:pointer (:struct point3d-t))))

(export 'triangle-contains-point)

;;; ----------------------------------------------------------------------------
;;; graphene_triangle_equal ()
;;;
;;; bool
;;; graphene_triangle_equal (const graphene_triangle_t *a,
;;;                          const graphene_triangle_t *b);
;;;
;;; Checks whether the two given graphene_triangle_t are equal.
;;;
;;; a
;;;     a graphene_triangle_t
;;;
;;; b
;;;     a graphene_triangle_t
;;;
;;; Returns
;;;     true if the triangles are equal
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_triangle_equal" triangle-equal) :bool
  (a (:pointer (:struct triangle-t)))
  (b (:pointer (:struct triangle-t))))

(export 'triangle-equal)

;;; --- End of file graphene.triangle.lisp -------------------------------------
