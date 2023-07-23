(in-package :graphene-test)

(def-suite graphene-ray :in graphene-suite)
(in-suite graphene-ray)

;;; --- Types and Values -------------------------------------------------------

;;;     graphene_ray_t
;;;     graphene_ray_intersection_kind_t

;;; --- Macros -----------------------------------------------------------------

(test with-graphene-ray.1
  (graphene:with-graphene-point3d (p 0 0 0)
    (graphene:with-graphene-vec3 (v 1 0 0)
      (graphene:with-graphene-ray (ray)
        (is (cffi:pointerp ray))
        (is (cffi:pointer-eq ray (graphene:ray-init ray p v)))
))))

(test with-graphene-ray.2
  (graphene:with-graphene-rays (ray1 (ray2 ray1) (ray3 (ray2 graphene:ray-t)))
    (is (cffi:pointerp ray1))
    (is (cffi:pointerp ray2))
))

(test with-graphene-ray.3
  (graphene:with-graphene-point3d (p)
    (graphene:with-graphene-vec3 (v)
      (graphene:with-graphene-ray (ray p v)
        (is (cffi:pointerp ray))
))))

(test with-graphene-ray.4
  (graphene:with-graphene-point3d (p)
    (graphene:with-graphene-vec3 (v)
      (graphene:with-graphene-rays (ray (ray1 ray)
                               (ray2 (ray graphene:ray-t))
                               (ray3 p v)
                               (ray4 (p graphene:point3d-t) v)
                               (ray5 (p graphene:point3d-t) (v graphene:vec3-t))
                               (ray6 (v graphene:vec3-t) v)
                               (ray7 (v graphene:vec3-t) (v graphene:vec3-t))
)
        (is (cffi:pointerp ray))
))))

;;; --- Functions --------------------------------------------------------------

;;;     graphene_ray_alloc
;;;     graphene_ray_free

;;;     graphene_ray_init

(test ray-init
  (graphene:with-graphene-point3d (p 0 0 0)
    (graphene:with-graphene-vec3 (v 0 0 0)
      (graphene:with-graphene-ray (ray)
        (is (cffi:pointerp ray))
        (is (cffi:pointer-eq ray (graphene:ray-init ray p v)))
))))

;;;     graphene_ray_init_from_ray

(test ray-init-from-ray
  (graphene:with-graphene-rays (ray source)
    (is (cffi:pointerp ray))
    (is (cffi:pointer-eq ray (graphene:ray-init-from-ray ray source)))
))

;;;     graphene_ray_init_from_vec3

(test ray-init-from-vec3
  (graphene:with-graphene-vec3s ((v1 1 2 3) (v2 4 5 6))
    (graphene:with-graphene-ray (ray)
      (is (cffi:pointerp ray))
      (is (cffi:pointer-eq ray (graphene:ray-init-from-vec3 ray v1 v2)))
)))

;;;     graphene_ray_get_origin
;;;     graphene_ray_get_direction

(test ray-origin/direction
  (graphene:with-graphene-point3ds ((origin 1.5 2.5 3.5) p)
    (graphene:with-graphene-vec3s ((direction 1.0 0.0 0.0) v)
      (graphene:with-graphene-ray (ray)
        (is (cffi:pointer-eq ray (graphene:ray-init ray origin direction)))

        (is (cffi:pointer-eq p (graphene:ray-origin ray p)))
        (is (= 1.5 (graphene:point3d-x p)))
        (is (= 2.5 (graphene:point3d-y p)))
        (is (= 3.5 (graphene:point3d-z p)))

        (is (cffi:pointer-eq v (graphene:ray-direction ray v)))
        (is (approx-equal 1.0 (graphene:vec3-x v)))
        (is (approx-equal 0.0 (graphene:vec3-y v)))
        (is (approx-equal 0.0 (graphene:vec3-z v)))

))))



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
