(in-package :graphene-test)

(def-suite graphene-ray :in graphene-suite)
(in-suite graphene-ray)

;;; --- Types and Values -------------------------------------------------------

;;;     graphene_ray_t
;;;     graphene_ray_intersection_kind_t

;;; --- Macros -----------------------------------------------------------------

(test with-graphene-ray.1
  (with-graphene-point3d (p 0 0 0)
    (with-graphene-vec3 (v 1 0 0)
      (with-graphene-ray (ray)
        (is (pointerp ray))
        (is (pointer-eq ray (ray-init ray p v)))
))))

(test with-graphene-ray.2
  (with-graphene-rays (ray1 (ray2 ray1) (ray3 (ray2 ray-t)))
    (is (pointerp ray1))
    (is (pointerp ray2))
))

(test with-graphene-ray.3
  (with-graphene-point3d (p)
    (with-graphene-vec3 (v)
      (with-graphene-ray (ray p v)
        (is (pointerp ray))
))))

(test with-graphene-ray.4
  (with-graphene-point3d (p)
    (with-graphene-vec3 (v)
      (with-graphene-rays (ray (ray1 ray)
                               (ray2 (ray ray-t))
                               (ray3 p v)
                               (ray4 (p point3d-t) v)
                               (ray5 (p point3d-t) (v vec3-t))
                               (ray6 (v vec3-t) v)
                               (ray7 (v vec3-t) (v vec3-t))
)
        (is (pointerp ray))
))))

;;; --- Functions --------------------------------------------------------------

;;;     graphene_ray_alloc
;;;     graphene_ray_free

;;;     graphene_ray_init

(test ray-init
  (with-graphene-point3d (p 0 0 0)
    (with-graphene-vec3 (v 0 0 0)
      (with-graphene-ray (ray)
        (is (pointerp ray))
        (is (pointer-eq ray (ray-init ray p v)))
))))

;;;     graphene_ray_init_from_ray

(test ray-init-from-ray
  (with-graphene-rays (ray source)
    (is (pointerp ray))
    (is (pointer-eq ray (ray-init-from-ray ray source)))
))

;;;     graphene_ray_init_from_vec3

(test ray-init-from-vec3
  (with-graphene-vec3s ((v1 1 2 3) (v2 4 5 6))
    (with-graphene-ray (ray)
      (is (pointerp ray))
      (is (pointer-eq ray (ray-init-from-vec3 ray v1 v2)))
)))

;;;     graphene_ray_get_origin
;;;     graphene_ray_get_direction

(test ray-origin/direction
  (with-graphene-point3ds ((origin 1.5 2.5 3.5) p)
    (with-graphene-vec3s ((direction 1.0 0.0 0.0) v)
      (with-graphene-ray (ray)
        (is (pointer-eq ray (ray-init ray origin direction)))

        (is (pointer-eq p (ray-origin ray p)))
        (is (= 1.5 (point3d-x p)))
        (is (= 2.5 (point3d-y p)))
        (is (= 3.5 (point3d-z p)))

        (is (pointer-eq v (ray-direction ray v)))
        (is (approx-equal 1.0 (vec3-x v)))
        (is (approx-equal 0.0 (vec3-y v)))
        (is (approx-equal 0.0 (vec3-z v)))

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
