(in-package :graphene-test)

(def-suite graphene-sphere :in graphene-suite)
(in-suite graphene-sphere)

;;; --- Types and Values -------------------------------------------------------

;;;     graphene_sphere_t

;;; --- Macros -----------------------------------------------------------------

(test graphene-with-sphere.1
  (graphene:with-objects ((center graphene:point3d-t)
                          (vec graphene:vec3-t))
    (graphene:with-sphere (sphere)
      (is (cffi:pointer-eq center (graphene:sphere-center sphere center)))
      (is (equal '(0.0 0.0 0.0)
                 (graphene:vec3-to-float (graphene:point3d-to-vec3 center vec))))
      (is (= 0 (graphene:sphere-radius sphere))))))

(test graphene-with-sphere.2
  (graphene:with-objects ((center graphene:point3d-t 0.0 0.0 0.0)
                          (vec graphene:vec3-t))
    (graphene:with-sphere (sphere center 1.0)
      (is (cffi:pointer-eq center (graphene:sphere-center sphere center)))
      (is (equal '(0.0 0.0 0.0)
                 (graphene:vec3-to-float (graphene:point3d-to-vec3 center vec))))
      (is (= 1.0 (graphene:sphere-radius sphere))))))

(test graphene-with-spheres
  (graphene:with-objects ((center graphene:point3d-t 0.0 0.0 0.0)
                          (vec graphene:vec3-t))
    (graphene:with-spheres ((sphere1 center 2.0)
                            (sphere2 sphere1))
      (is (cffi:pointer-eq center (graphene:sphere-center sphere2 center)))
      (is (equal '(0.0 0.0 0.0)
                 (graphene:vec3-to-float (graphene:point3d-to-vec3 center vec))))
      (is (= 2.0 (graphene:sphere-radius sphere2))))))

;;; --- Functions --------------------------------------------------------------

;;;     graphene_sphere_alloc
;;;     graphene_sphere_free

(test graphene-sphere-alloc/free
  (let ((sphere nil))
    (is (cffi:pointerp (setf sphere (graphene:sphere-alloc))))
    (is-false (graphene:sphere-free sphere))))

;;;     graphene_sphere_init

(test graphene-sphere-init
  (let ((sphere (graphene:sphere-alloc)))
    (graphene:with-point3d (center 0 0 0)
      (is (cffi:pointer-eq sphere (graphene:sphere-init sphere center 1))))
      (is-false (graphene:sphere-free sphere))))

;;;     graphene_sphere_init_from_points

(test graphene-sphere-init-from-points.1
  (graphene:with-sphere (sphere)
    (graphene:with-point3ds ((p1 1 0 0)
                             (p2 0 1 0)
                             (p3 0 0 1)
                             (center 0 0 0)
                             result)
      (let ((points (list p1 p2 p3)))
        (is (cffi:pointer-eq sphere
                             (graphene:sphere-init-from-points sphere
                                                               points
                                                               center)))
        (is (cffi:pointer-eq result
                             (graphene:sphere-center sphere result)))
        (is (= 0 (graphene:point3d-x result)))
        (is (= 0 (graphene:point3d-y result)))
        (is (= 0 (graphene:point3d-z result)))
        (is (= 1.0 (graphene:sphere-radius sphere)))))))

(test graphene-sphere-init-from-points.2
  (graphene:with-sphere (sphere)
    (graphene:with-point3ds ((p1 1 0 0)
                             (p2 0 1 0)
                             (p3 0 0 1)
                             (p4 -1 0 0)
                             (p5 0 -1 0)
                             (p6 0 0 -1)
                             result)
      (let ((points (list p1 p2 p3 p4 p5 p6))
            (center nil))
        (is (cffi:pointer-eq sphere
                             (graphene:sphere-init-from-points sphere
                                                               points
                                                               center)))
        (is (cffi:pointer-eq result
                             (graphene:sphere-center sphere result)))
        (is (= 0 (graphene:point3d-x result)))
        (is (= 0 (graphene:point3d-y result)))
        (is (= 0 (graphene:point3d-z result)))
        (is (= 1.0 (graphene:sphere-radius sphere)))))))

;;;     graphene_sphere_init_from_vectors

(test graphene-sphere-init-from-vectors.1
  (graphene:with-sphere (sphere)
    (graphene:with-vec3s ((v1 1 0 0)
                          (v2 0 1 0)
                          (v3 0 0 1)
                          (center 0 0 0)
                          result)
      (let ((vectors (list v1 v2 v3)))
        (is (cffi:pointer-eq sphere
                             (graphene:sphere-init-from-vectors sphere
                                                                vectors
                                                                center)))
        (is (cffi:pointer-eq result
                             (graphene:sphere-center sphere result)))
        (is (= 0 (graphene:point3d-x result)))
        (is (= 0 (graphene:point3d-y result)))
        (is (= 0 (graphene:point3d-z result)))
        (is (= 1.0 (graphene:sphere-radius sphere)))))))

(test graphene-sphere-init-from-vectors.2
  (graphene:with-sphere (sphere)
    (graphene:with-vec3s ((v1 1 0 0)
                          (v2 0 1 0)
                          (v3 0 0 1)
                          (v4 -1 0 0)
                          (v5 0 -1 0)
                          (v6 0 0 -1)
                          result)
      (let ((vectors (list v1 v2 v3 v4 v5 v6))
            (center nil))
        (is (cffi:pointer-eq sphere
                             (graphene:sphere-init-from-vectors sphere
                                                                vectors
                                                                center)))
        (is (cffi:pointer-eq result
                             (graphene:sphere-center sphere result)))
        (is (= 0 (graphene:point3d-x result)))
        (is (= 0 (graphene:point3d-y result)))
        (is (= 0 (graphene:point3d-z result)))
        (is (= 1.0 (graphene:sphere-radius sphere)))))))

;;;     graphene_sphere_get_center
;;;     graphene_sphere_get_radius

(test graphene-sphere-center/radius
  (graphene:with-objects ((center graphene:point3d-t 1.0 2.0 3.0)
                          (result graphene:point3d-t)
                          (vector graphene:vec3-t))
    (graphene:with-sphere (sphere center 1.0)
      (is (= 1.0 (graphene:sphere-radius sphere)))
      (is (cffi:pointer-eq result (graphene:sphere-center sphere result)))
      (is (equal '(1.0 2.0 3.0)
                 (graphene:vec3-to-float
                     (graphene:point3d-to-vec3 result vector)))))))

;;;     graphene_sphere_get_bounding_box

(test graphene-sphere-bounding-box
  (graphene:with-objects ((center graphene:point3d-t 1.0 2.0 3.0)
                          (box graphene:box-t))
    (graphene:with-sphere (sphere center 1.0)
      (is (cffi:pointer-eq box (graphene:sphere-bounding-box sphere box)))
      (is (= 2.0 (graphene:box-width box)))
      (is (= 2.0 (graphene:box-height box)))
      (is (= 2.0 (graphene:box-depth box))))))

;;;     graphene_sphere_is_empty

(test graphene-sphere-is-empty
  (graphene:with-objects ((center graphene:point3d-t 1.0 2.0 3.0)
                          (box graphene:box-t))
    (graphene:with-sphere (sphere center 0.0)
      (is-true (graphene:sphere-is-empty sphere))
      (is (cffi:pointer-eq sphere (graphene:sphere-init sphere center 1.0)))
      (is-false (graphene:sphere-is-empty sphere)))))

;;;     graphene_sphere_distance

(test graphene-sphere-distance
  (graphene:with-object (center graphene:point3d-t 0.0 0.0 0.0)
    (graphene:with-sphere (sphere center 1.0)
      (is (= -1.0 (graphene:sphere-distance sphere center))))))

;;;     graphene_sphere_contains_point

(test graphene-sphere-contains-point
  (graphene:with-object (center graphene:point3d-t 0.0 0.0 0.0)
    (graphene:with-sphere (sphere center 1.0)
      (is-true (graphene:sphere-contains-point sphere center)))))

;;;     graphene_sphere_translate

(test graphene-sphere-translate
  (graphene:with-objects ((center graphene:point3d-t 0.5 1.0 1.5)
                          (point graphene:point3d-t 1 2 3))
    (graphene:with-spheres ((sphere center 1.0) result)
      (is (cffi:pointer-eq result
                           (graphene:sphere-translate sphere point result)))
      (is (cffi:pointer-eq center
                           (graphene:sphere-center result center)))
      (is (= 1.5 (graphene:point3d-x center)))
      (is (= 3.0 (graphene:point3d-y center)))
      (is (= 4.5 (graphene:point3d-z center))))))

;;;     graphene_sphere_equal

(test graphene-sphere-equal
  (graphene:with-object (center graphene:point3d-t 0.0 0.0 0.0)
    (graphene:with-spheres ((sphere1 center 1.0)
                            (sphere2 center 1.0)
                            (sphere3 center 0.5))
      (is (graphene:sphere-equal sphere1 sphere2))
      (is (not (graphene:sphere-equal sphere1 sphere3))))))

;;; 2023-12-7
