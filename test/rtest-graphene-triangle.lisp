(in-package :graphene-test)

(def-suite graphene-triangle :in graphene-suite)
(in-suite graphene-triangle)

;;; --- Types and Values -------------------------------------------------------

;;;     graphene_triangle_t

;;; --- Macros -----------------------------------------------------------------

(test graphene-with-triangle.1
  (graphene:with-triangle (triangle)
    (is (cffi:pointerp triangle))))

(test graphene-with-triangle.2
  (graphene:with-point3ds ((p1 0 0 0) (p2 1 0 0) (p3 0 1 0) a b c)
    (graphene:with-triangle (triangle p1 p2 p3)
      (is (cffi:pointer-eq a (graphene:triangle-points triangle a b c)))
      (is (graphene:point3d-equal p1 a))
      (is (graphene:point3d-equal p2 b))
      (is (graphene:point3d-equal p3 c)))))

(test graphene-with-triangle.3
  (graphene:with-objects ((v1 graphene:vec3-t 0 0 0)
                          (v2 graphene:vec3-t 1 0 0)
                          (v3 graphene:vec3-t 0 1 0)
                          (v graphene:vec4-t)
                          (a graphene:point3d-t)
                          (b graphene:point3d-t)
                          (c graphene:point3d-t))
    (graphene:with-triangle (triangle (v1 graphene:vec3-t) v2 v3)
      (is (cffi:pointer-eq a (graphene:triangle-points triangle a b c)))
      (is (graphene:point3d-equal v1 (graphene:point3d-to-vec3 a v)))
      (is (graphene:point3d-equal v2 (graphene:point3d-to-vec3 b v)))
      (is (graphene:point3d-equal v3 (graphene:point3d-to-vec3 c v))))))

;;; --- Functions --------------------------------------------------------------

;;;     graphene_triangle_alloc
;;;     graphene_triangle_free
;;;     graphene_triangle_init_from_point3d
;;;     graphene_triangle_init_from_vec3

;;;     graphene_triangle_init_from_float

(test graphene-triangle-init-from-float
  (let ((triangle (graphene:triangle-alloc)))
    (graphene:with-objects ((a graphene:point3d-t)
                            (b graphene:point3d-t)
                            (c graphene:point3d-t)
                            (v graphene:vec3-t))
    (is (cffi:pointer-eq triangle
                         (graphene:triangle-init-from-float triangle
                                                            '(1 2 3)
                                                            '(4 5 6)
                                                            '(7 8 9))))
    (is (every #'cffi:pointerp
               (multiple-value-list (graphene:triangle-points triangle a b c))))
    (is (equal '(1.0 2.0 3.0)
               (graphene:vec3-to-float (graphene:point3d-to-vec3 a v))))
    (is (equal '(4.0 5.0 6.0)
               (graphene:vec3-to-float (graphene:point3d-to-vec3 b v))))
    (is (equal '(7.0 8.0 9.0)
               (graphene:vec3-to-float (graphene:point3d-to-vec3 c v))))
    (is-false (graphene:triangle-free triangle)))))

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

;;; 2023-12-7
