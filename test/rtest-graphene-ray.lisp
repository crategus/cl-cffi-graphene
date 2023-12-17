(in-package :graphene-test)

(def-suite graphene-ray :in graphene-suite)
(in-suite graphene-ray)

;;; --- Types and Values -------------------------------------------------------

;;;     graphene_ray_intersection_kind_t
;;;     graphene_ray_t

;;; --- Macros -----------------------------------------------------------------

(test graphene-with-ray.1
  (graphene:with-point3d (p 0 0 0)
    (graphene:with-vec3 (v 1 0 0)
      (graphene:with-ray (ray)
        (is (cffi:pointer-eq ray (graphene:ray-init ray p v)))))))

(test graphene-with-ray.2
  (graphene:with-rays (ray1 (ray2 ray1) (ray3 (ray2 graphene:ray-t)))
    (is (cffi:pointerp ray1))
    (is (cffi:pointerp ray2))
    (is (cffi:pointerp ray3))))

(test graphene-with-ray.3
  (graphene:with-point3d (p)
    (graphene:with-vec3 (v)
      (graphene:with-ray (ray p v)
        (is (cffi:pointerp ray))))))

(test with-ray.4
  (graphene:with-point3d (p)
    (graphene:with-vec3 (v)
      (graphene:with-rays (ray1
                           (ray2 ray1)
                           (ray3 (ray1 graphene:ray-t))
                           (ray4 p v)
                           (ray5 (p graphene:point3d-t) v)
                           (ray6 (p graphene:point3d-t) (v graphene:vec3-t))
                           (ray7 (v graphene:vec3-t) v)
                           (ray8 (v graphene:vec3-t) (v graphene:vec3-t)))
        (is (cffi:pointerp ray1))
        (is (cffi:pointerp ray2))
        (is (cffi:pointerp ray3))
        (is (cffi:pointerp ray4))
        (is (cffi:pointerp ray5))
        (is (cffi:pointerp ray6))
        (is (cffi:pointerp ray7))
        (is (cffi:pointerp ray8))))))

;;; --- Functions --------------------------------------------------------------

;;;     graphene_ray_alloc
;;;     graphene_ray_free

;;;     graphene_ray_init

(test graphene-ray-init
  (graphene:with-objects ((p graphene:point3d-t 0 0 0)
                          (v graphene:vec3-t 1 0 0)
                          (origin graphene:point3d-t)
                          (direction graphene:vec3-t))
    (graphene:with-ray (ray)
      (is (cffi:pointerp ray))
      (is (cffi:pointer-eq ray (graphene:ray-init ray p v)))
      ;; Check the origin
      (is (cffi:pointer-eq origin (graphene:ray-origin ray origin)))
      (is (graphene:point3d-equal p (graphene:ray-origin ray origin)))
      ;; Check the direction
      (is (cffi:pointer-eq direction (graphene:ray-direction ray direction)))
      (is (graphene:vec3-equal v (graphene:ray-direction ray direction))))))

;;;     graphene_ray_init_from_ray

(test graphene-ray-init-from-ray
  (graphene:with-objects ((p graphene:point3d-t 0 0 0)
                          (v graphene:vec3-t 1 0 0)
                          (origin graphene:point3d-t)
                          (direction graphene:vec3-t))
    (graphene:with-rays (ray source)
      (is (cffi:pointer-eq source (graphene:ray-init source p v)))
      (is (cffi:pointer-eq ray (graphene:ray-init-from-ray ray source)))
      ;; Check the origin
      (is (cffi:pointer-eq origin (graphene:ray-origin ray origin)))
      (is (graphene:point3d-equal p (graphene:ray-origin ray origin)))
      ;; Check the direction
      (is (cffi:pointer-eq direction (graphene:ray-direction ray direction)))
      (is (graphene:vec3-equal v (graphene:ray-direction ray direction))))))

;;;     graphene_ray_init_from_vec3

(test graphene-ray-init-from-vec3
  (graphene:with-objects ((v1 graphene:vec3-t 0 0 0)
                          (v2 graphene:vec3-t 1 0 0)
                          (v3 graphene:vec3-t)
                          (origin graphene:point3d-t)
                          (direction graphene:vec3-t))
    (graphene:with-ray (ray)
      (is (cffi:pointer-eq ray (graphene:ray-init-from-vec3 ray v1 v2)))
      ;; Check the origin
      (is (cffi:pointer-eq origin (graphene:ray-origin ray origin)))
      (is (graphene:point3d-equal (graphene:point3d-to-vec3 origin v3)
                                  (graphene:ray-origin ray origin)))
      ;; Check the direction
      (is (cffi:pointer-eq direction (graphene:ray-direction ray direction)))
      (is (graphene:vec3-equal v2 (graphene:ray-direction ray direction))))))

;;;     graphene_ray_get_origin
;;;     graphene_ray_get_direction

(test graphene-ray-origin/direction
  (graphene:with-objects ((origin graphene:point3d-t 1.5 2.5 3.5)
                          (direction graphene:vec3-t 1.0 0.0 0.0)
                          (v graphene:vec3-t)
                          (p graphene:point3d-t))
    (graphene:with-ray (ray)
      (is (cffi:pointer-eq ray (graphene:ray-init ray origin direction)))
      (is (cffi:pointer-eq p (graphene:ray-origin ray p)))
      (is (= 1.5 (graphene:point3d-x p)))
      (is (= 2.5 (graphene:point3d-y p)))
      (is (= 3.5 (graphene:point3d-z p)))
      (is (cffi:pointer-eq v (graphene:ray-direction ray v)))
      (is (approx-equal 1.0 (graphene:vec3-x v)))
      (is (approx-equal 0.0 (graphene:vec3-y v)))
      (is (approx-equal 0.0 (graphene:vec3-z v))))))

;;;     graphene_ray_get_position_at

(test graphene-ray-position-at
  (graphene:with-objects ((origin graphene:point3d-t 0.0 0.0 0.0)
                          (direction graphene:vec3-t 1.0 1.0 1.0)
                          (ray graphene:ray-t origin direction)
                          (pos graphene:point3d-t))
    ;; Check at 1.0
    (is (cffi:pointer-eq pos (graphene:ray-position-at ray 1.0 pos)))
    (is (= (/ 1 (sqrt 3)) (graphene:point3d-x pos)))
    (is (= (/ 1 (sqrt 3)) (graphene:point3d-y pos)))
    (is (= (/ 1 (sqrt 3)) (graphene:point3d-z pos)))
    ;; Check at 2.0
    (is (cffi:pointer-eq pos (graphene:ray-position-at ray 2.0 pos)))
    (is (= (/ 2 (sqrt 3)) (graphene:point3d-x pos)))
    (is (= (/ 2 (sqrt 3)) (graphene:point3d-y pos)))
    (is (= (/ 2 (sqrt 3)) (graphene:point3d-z pos)))))

;;;     graphene_ray_get_distance_to_point

(test graphene-ray-distance-to-point
  (graphene:with-objects ((origin graphene:point3d-t 0.0 0.0 0.0)
                          (direction graphene:vec3-t 1.0 0.0 0.0)
                          (ray graphene:ray-t origin direction)
                          (point graphene:point3d-t 1.0 0.0 0.0))
    (is (approx-equal 0.0 (graphene:ray-distance-to-point ray point)))
    (is (cffi:pointer-eq point (graphene:point3d-init point 1.0 1.0 0.0)))
    (is (approx-equal 1.0 (graphene:ray-distance-to-point ray  point)))
    (is (cffi:pointer-eq point (graphene:point3d-init point 1.0 0.0 1.0)))
    (is (approx-equal 1.0 (graphene:ray-distance-to-point ray  point)))))

;;;     graphene_ray_get_distance_to_plane

;; TODO: We get no distance. Why? Check the implementation of plane-t

#+nil
(test graphene-ray-distance-to-plane
  (graphene:with-objects ((origin graphene:point3d-t 0.0 0.0 0.0)
                          (direction graphene:vec3-t 1.0 0.0 0.0)
                          (ray graphene:ray-t origin direction)
                          (normal graphene:vec3-t 1.0 1.0 1.0)
                          (plane graphene:plane-t normal 2.0))
    (is-false (graphene:ray-distance-to-plane plane plane))
))

;;;     graphene_ray_get_closest_point_to_point
;;;     graphene_ray_equal

;;;     graphene_ray_intersect_sphere
;;;     graphene_ray_intersects_sphere

(test graphene-ray-intersect-sphere
  (graphene:with-objects ((origin graphene:point3d-t 0.0 0.0 0.0)
                          (direction graphene:vec3-t 1.0 0.0 0.0)
                          (ray graphene:ray-t origin direction)
                          (center graphene:point3d-t 0.0 0.0 0.0)
                          (sphere graphene:sphere-t center 1.5))
    (is-true (graphene:ray-intersects-sphere ray sphere))
    (is (equal '(:leave 1.5)
               (multiple-value-list
                   (graphene:ray-intersect-sphere ray sphere))))))

;;;     graphene_ray_intersect_box
;;;     graphene_ray_intersects_box
;;;     graphene_ray_intersect_triangle
;;;     graphene_ray_intersects_triangle

;;; 2023-12-6
