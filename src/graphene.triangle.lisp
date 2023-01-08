;;; ----------------------------------------------------------------------------
;;; graphene.triangle.lisp
;;;
;;; The documentation of this file is taken from the GRAPHENE Reference Manual
;;; and modified to document the Lisp binding to the Graphene library.
;;; See <https://ebassi.github.io/graphene/docs/>.
;;; The API documentation of the Lisp binding is available from
;;; <http://www.crategus.com/books/cl-cffi-graphene/>.
;;;
;;; Copyright (C) 2022 Dieter Kaiser
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License for Lisp
;;; as published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version and with a preamble to
;;; the GNU Lesser General Public License that clarifies the terms for use
;;; with Lisp programs and is referred as the LLGPL.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program and the preamble to the Gnu Lesser
;;; General Public License.  If not, see <http://www.gnu.org/licenses/>
;;; and <http://opensource.franz.com/preamble.html>.
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
;;;
;;; Description
;;;
;;;     graphene_triangle_t represents a triangle in 3D space.
;;; ----------------------------------------------------------------------------

(in-package :graphene)

;;; ----------------------------------------------------------------------------
;;; graphene_triangle_t
;;;
;;; typedef struct {
;;; } graphene_triangle_t;
;;;
;;; A triangle.
;;;
;;; Since: 1.2
;;; ----------------------------------------------------------------------------

(defcstruct triangle-t)

(export 'triangle-t)

;;; ----------------------------------------------------------------------------
;;;graphene_triangle_alloc ()
;;; ----------------------------------------------------------------------------

(defcfun ("graphene_triangle_alloc" triangle-alloc)
    (:pointer (:struct triangle-t))
 #+liber-documentation
 "@version{#2022-9-20}
  @return{The newly allocated @symbol{triangle-t} instance. Use the
    @fun{triangle-free} function to free the resources allocated by this
    function.}
  @begin{short}
    Allocates a new @symbol{triangle-t} instance.
  @end{short}
  The contents of the returned instance are undefined.
  @see-symbol{triangle-t}
  @see-function{triangle-free}")

(export 'triangle-alloc)

;;; ----------------------------------------------------------------------------
;;;graphene_triangle_free ()
;;; ----------------------------------------------------------------------------

(defcfun ("graphene_triangle_free" triangle-free) :void
 #+liber-documentation
 "@version{#2022-9-20}
  @argument[t]{a @symbol{triangle-t} instance}
  @begin{short}
    Frees the resources allocated by the @fun{triangle-alloc} function.
  @end{short}
  @see-symbol{triangle-t}
  @see-function{triangle-alloc}"
  (tr (:pointer (:struct triangle-t))))

(export 'triangle-free)

;;; ----------------------------------------------------------------------------
;;;graphene_triangle_init_from_point3d ()
;;;graphene_triangle_t *
;;;graphene_triangle_init_from_point3d (graphene_triangle_t *t,
;;;                                     const graphene_point3d_t *a,
;;;                                     const graphene_point3d_t *b,
;;;                                     const graphene_point3d_t *c);
;;;Initializes a graphene_triangle_t using the three given 3D points.

;;;Parameters
;;;t

;;;the graphene_triangle_t to initialize

;;;a

;;;a graphene_point3d_t.

;;;b

;;;a graphene_point3d_t.

;;;c

;;;a graphene_point3d_t.

;;;Returns
;;;the initialized graphene_triangle_t.

;;;[transfer none]

;;;Since: 1.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;graphene_triangle_init_from_vec3 ()
;;;graphene_triangle_t *
;;;graphene_triangle_init_from_vec3 (graphene_triangle_t *t,
;;;                                  const graphene_vec3_t *a,
;;;                                  const graphene_vec3_t *b,
;;;                                  const graphene_vec3_t *c);
;;;Initializes a graphene_triangle_t using the three given vectors.

;;;Parameters
;;;t

;;;the graphene_triangle_t to initialize

;;;a

;;;a graphene_vec3_t.

;;;b

;;;a graphene_vec3_t.

;;;c

;;;a graphene_vec3_t.

;;;Returns
;;;the initialized graphene_triangle_t.

;;;[transfer none]

;;;Since: 1.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;graphene_triangle_init_from_float ()
;;;graphene_triangle_t *
;;;graphene_triangle_init_from_float (graphene_triangle_t *t,
;;;                                   const float *a,
;;;                                   const float *b,
;;;                                   const float *c);
;;;Initializes a graphene_triangle_t using the three given arrays of floating point values, each representing the coordinates of a point in 3D space.

;;;Parameters
;;;t

;;;the graphene_triangle_t to initialize

;;;a

;;;an array of 3 floating point values.

;;;b

;;;an array of 3 floating point values.

;;;c

;;;an array of 3 floating point values.

;;;Returns
;;;the initialized graphene_triangle_t.

;;;[transfer none]

;;;Since: 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;graphene_triangle_get_points ()
;;;void
;;;graphene_triangle_get_points (const graphene_triangle_t *t,
;;;                              graphene_point3d_t *a,
;;;                              graphene_point3d_t *b,
;;;                              graphene_point3d_t *c);
;;;Retrieves the three vertices of the given graphene_triangle_t and returns their coordinates as graphene_point3d_t.

;;;Parameters
;;;t

;;;a graphene_triangle_t

;;;a

;;;return location for the coordinates of the first vertex.

;;;b

;;;return location for the coordinates of the second vertex.

;;;c

;;;return location for the coordinates of the third vertex.

;;;Since: 1.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;graphene_triangle_get_vertices ()
;;;void
;;;graphene_triangle_get_vertices (const graphene_triangle_t *t,
;;;                                graphene_vec3_t *a,
;;;                                graphene_vec3_t *b,
;;;                                graphene_vec3_t *c);
;;;Retrieves the three vertices of the given graphene_triangle_t.

;;;Parameters
;;;t

;;;a graphene_triangle_t

;;;a

;;;return location for the first vertex.

;;;b

;;;return location for the second vertex.

;;;c

;;;return location for the third vertex.

;;;Since: 1.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;graphene_triangle_get_area ()
;;;float
;;;graphene_triangle_get_area (const graphene_triangle_t *t);
;;;Computes the area of the given graphene_triangle_t.

;;;Parameters
;;;t

;;;a graphene_triangle_t

;;;Returns
;;;the area of the triangle

;;;Since: 1.2

;;;graphene_triangle_get_midpoint ()
;;;void
;;;graphene_triangle_get_midpoint (const graphene_triangle_t *t,
;;;                                graphene_point3d_t *res);
;;;Computes the coordinates of the midpoint of the given graphene_triangle_t.

;;;The midpoint G is the centroid of the triangle, i.e. the intersection of its medians.

;;;Parameters
;;;t

;;;a graphene_triangle_t

;;;res

;;;return location for the coordinates of the midpoint.

;;;Since: 1.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;graphene_triangle_get_normal ()
;;;void
;;;graphene_triangle_get_normal (const graphene_triangle_t *t,
;;;                              graphene_vec3_t *res);
;;;Computes the normal vector of the given graphene_triangle_t.

;;;Parameters
;;;t

;;;a graphene_triangle_t

;;;res

;;;return location for the normal vector.

;;;Since: 1.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;graphene_triangle_get_plane ()
;;;void
;;;graphene_triangle_get_plane (const graphene_triangle_t *t,
;;;                             graphene_plane_t *res);
;;;Computes the plane based on the vertices of the given graphene_triangle_t.

;;;Parameters
;;;t

;;;a graphene_triangle_t

;;;res

;;;return location for the plane.

;;;Since: 1.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;graphene_triangle_get_bounding_box ()
;;;void
;;;graphene_triangle_get_bounding_box (const graphene_triangle_t *t,
;;;                                    graphene_box_t *res);
;;;Computes the bounding box of the given graphene_triangle_t.

;;;Parameters
;;;t

;;;a graphene_triangle_t

;;;res

;;;return location for the box.

;;;Since: 1.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;graphene_triangle_get_barycoords ()
;;;bool
;;;graphene_triangle_get_barycoords (const graphene_triangle_t *t,
;;;                                  const graphene_point3d_t *p,
;;;                                  graphene_vec2_t *res);
;;;Computes the barycentric coordinates of the given point p .

;;;The point p must lie on the same plane as the triangle t ; if the point is not coplanar, the result of this function is undefined.

;;;If we place the origin in the coordinates of the triangle's A point, the barycentric coordinates are u, which is on the AC vector; and v which is on the AB vector:



;;;The returned graphene_vec2_t contains the following values, in order:

;;;res.x = u

;;;res.y = v

;;;Parameters
;;;t

;;;a graphene_triangle_t

;;;p

;;;a graphene_point3d_t.

;;;res

;;;return location for the vector with the barycentric coordinates.

;;;Returns
;;;true if the barycentric coordinates are valid

;;;Since: 1.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;graphene_triangle_get_uv ()
;;;bool
;;;graphene_triangle_get_uv (const graphene_triangle_t *t,
;;;                          const graphene_point3d_t *p,
;;;                          const graphene_vec2_t *uv_a,
;;;                          const graphene_vec2_t *uv_b,
;;;                          const graphene_vec2_t *uv_c,
;;;                          graphene_vec2_t *res);
;;;Computes the UV coordinates of the given point p .

;;;The point p must lie on the same plane as the triangle t ; if the point is not coplanar, the result of this function is undefined. If p is NULL, the point will be set in (0, 0, 0).

;;;The UV coordinates will be placed in the res vector:

;;;res.x = u

;;;res.y = v

;;;See also: graphene_triangle_get_barycoords()

;;;Parameters
;;;t

;;;a graphene_triangle_t

;;;p

;;;a graphene_point3d_t.

;;;uv_a

;;;the UV coordinates of the first point

;;;uv_b

;;;the UV coordinates of the second point

;;;uv_c

;;;the UV coordinates of the third point

;;;res

;;;a vector containing the UV coordinates of the given point p .

;;;Returns
;;;true if the coordinates are valid

;;;Since: 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;graphene_triangle_contains_point ()
;;;bool
;;;graphene_triangle_contains_point (const graphene_triangle_t *t,
;;;                                  const graphene_point3d_t *p);
;;;Checks whether the given triangle t contains the point p .

;;;Parameters
;;;t

;;;a graphene_triangle_t

;;;p

;;;a graphene_point3d_t

;;;Returns
;;;true if the point is inside the triangle

;;;Since: 1.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;graphene_triangle_equal ()
;;;bool
;;;graphene_triangle_equal (const graphene_triangle_t *a,
;;;                         const graphene_triangle_t *b);
;;;Checks whether the two given graphene_triangle_t are equal.

;;;Parameters
;;;a

;;;a graphene_triangle_t

;;;b

;;;a graphene_triangle_t

;;;Returns
;;;true if the triangles are equal

;;;Since: 1.2
;;; ----------------------------------------------------------------------------

;;; --- End of file graphene.triangle.lisp -------------------------------------
