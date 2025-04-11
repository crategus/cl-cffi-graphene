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

(test graphene-triangle-alloc/free
  (let (triangle)
    (is (cffi:pointerp (setf triangle (graphene:triangle-alloc))))
    (is-false (graphene:triangle-free triangle))))

;;;     graphene_triangle_init_from_point3d

(test graphene-triangle-init-from-point3d
  (let ((triangle (graphene:triangle-alloc)))
    (graphene:with-vec3 (v)
      (graphene:with-point3ds ((p1 0 0 0) (p2 2 0 0) (p3 1 1 0) a b c)
        (is (cffi:pointer-eq
                triangle
                (graphene:triangle-init-from-point3d triangle p1 p2 p3)))
        (is (every #'cffi:pointerp
                  (multiple-value-list
                      (graphene:triangle-points triangle a b c))))
        (is (equal '(0.0 0.0 0.0)
                   (graphene:vec3-to-float
                       (graphene:point3d-to-vec3 a v))))
        (is (equal '(2.0 0.0 0.0)
                   (graphene:vec3-to-float
                       (graphene:point3d-to-vec3 b v))))
        (is (equal '(1.0 1.0 0.0)
                   (graphene:vec3-to-float
                       (graphene:point3d-to-vec3 c v))))
        (is-false (graphene:triangle-free triangle))))))

;;;     graphene_triangle_init_from_vec3

(test graphene-triangle-init-from-vec3
  (let ((triangle (graphene:triangle-alloc)))
    (graphene:with-vec3s (v (v1 0 0 0) (v2 2 0 0) (v3 1 1 0))
      (graphene:with-point3ds (a b c)
        (is (cffi:pointer-eq
                triangle
                (graphene:triangle-init-from-vec3 triangle v1 v2 v3)))
          (is (every #'cffi:pointerp
                    (multiple-value-list
                        (graphene:triangle-points triangle a b c))))
          (is (equal '(0.0 0.0 0.0)
                     (graphene:vec3-to-float
                         (graphene:point3d-to-vec3 a v))))
          (is (equal '(2.0 0.0 0.0)
                     (graphene:vec3-to-float
                         (graphene:point3d-to-vec3 b v))))
          (is (equal '(1.0 1.0 0.0)
                     (graphene:vec3-to-float
                         (graphene:point3d-to-vec3 c v))))
          (is-false (graphene:triangle-free triangle))))))

;;;     graphene_triangle_init_from_float

(test graphene-triangle-init-from-float
  (let ((triangle (graphene:triangle-alloc)))
    (graphene:with-point3ds (a b c)
      (graphene:with-vec3 (v)
        (is (cffi:pointer-eq
                triangle
                (graphene:triangle-init-from-float triangle
                                                   '(0 0 0)
                                                   '(2 0 0)
                                                   '(1 1 0))))
        (is (every #'cffi:pointerp
                   (multiple-value-list
                       (graphene:triangle-points triangle a b c))))
        (is (equal '(0.0 0.0 0.0)
                   (graphene:vec3-to-float (graphene:point3d-to-vec3 a v))))
        (is (equal '(2.0 0.0 0.0)
                   (graphene:vec3-to-float (graphene:point3d-to-vec3 b v))))
        (is (equal '(1.0 1.0 0.0)
                   (graphene:vec3-to-float (graphene:point3d-to-vec3 c v))))
        (is-false (graphene:triangle-free triangle))))))

;;;     graphene_triangle_get_points

(test graphene-triangle-points
  (graphene:with-point3ds ((p1 0 0 0) (p2 2 0 0) (p3 1 1 0) a b c)
    (graphene:with-vec3 (v)
      (graphene:with-triangle (triangle p1 p2 p3)
        (is (every #'cffi:pointerp
                  (multiple-value-list
                      (graphene:triangle-points triangle a b c))))
        (is (equal '(0.0 0.0 0.0)
                   (graphene:vec3-to-float
                       (graphene:point3d-to-vec3 a v))))
        (is (equal '(2.0 0.0 0.0)
                   (graphene:vec3-to-float
                       (graphene:point3d-to-vec3 b v))))
        (is (equal '(1.0 1.0 0.0)
                   (graphene:vec3-to-float
                       (graphene:point3d-to-vec3 c v))))))))

;;;     graphene_triangle_get_vertices

(test graphene-triangle-vertices
  (graphene:with-vec3s ((v1 0 0 0) (v2 2 0 0) (v3 1 1 0) a b c)
    (graphene:with-triangle (triangle (v1 graphene:vec3-t) v2 v3)
      (is (every #'cffi:pointerp
                (multiple-value-list
                    (graphene:triangle-vertices triangle a b c))))
      (is (equal '(0.0 0.0 0.0)
                 (graphene:vec3-to-float a)))
      (is (equal '(2.0 0.0 0.0)
                 (graphene:vec3-to-float b)))
      (is (equal '(1.0 1.0 0.0)
                 (graphene:vec3-to-float c))))))

;;;     graphene_triangle_get_area

(test graphene-triangle-area
  (graphene:with-point3ds (p1 p2 p3)
    (graphene:with-triangle (triangle (graphene:point3d-init p1 0 0 0)
                                      (graphene:point3d-init p2 1 0 0)
                                      (graphene:point3d-init p3 0 1 0))
      (is (= 0.5 (graphene:triangle-area triangle))))))

;;;     graphene_triangle_get_midpoint

(test graphene-triangle-midpoint
  (graphene:with-point3ds ((p1 0 0 0) (p2 0 2 0) (p3 1 1 0) midpoint)
    (graphene:with-vec3 (v)
      (graphene:with-triangle (triangle p1 p2 p3)
        (is (every #'approx-equal
                   '(0.33333333 1.0 0.0)
                   (graphene:vec3-to-float
                       (graphene:point3d-to-vec3
                           (graphene:triangle-midpoint triangle midpoint)
                           v))))))))

;;;     graphene_triangle_get_normal

(test graphene-triangle-normal
  (graphene:with-point3ds ((p1 0 0 0) (p2 0 2 0) (p3 1 1 0))
    (graphene:with-vec3 (normal)
      (graphene:with-triangle (triangle p1 p2 p3)
        (is (cffi:pointer-eq normal
                             (graphene:triangle-normal triangle normal)))
          (is (every #'approx-equal
                     '(0.0 0.0 -1.0)
                     (graphene:vec3-to-float normal)))))))

;;;     graphene_triangle_get_plane

(test graphene-triangle-plane
  (graphene:with-point3ds ((p1 0 0 0) (p2 0 2 0) (p3 1 1 0))
    (graphene:with-vec3 (normal)
      (graphene:with-plane (plane)
        (graphene:with-triangle (triangle p1 p2 p3)
          (is (cffi:pointer-eq plane
                               (graphene:triangle-plane triangle plane)))
          (is (approx-equal 0.0 (graphene:plane-constant plane)))
          (is (every #'approx-equal
                     '(0.0 0.0 -1.0)
                     (graphene:vec3-to-float
                         (graphene:plane-normal plane normal)))))))))

;;;     graphene_triangle_get_bounding_box

(test graphene-triangle-bounding-box
  (graphene:with-point3ds ((p1 0 0 0) (p2 0 2 0) (p3 1 1 0))
    (graphene:with-box (box)
      (graphene:with-triangle (triangle p1 p2 p3)
        (is (cffi:pointer-eq box
                             (graphene:triangle-bounding-box triangle box)))
        (is (= 1.0 (graphene:box-width box)))
        (is (= 2.0 (graphene:box-height box)))))))

;;;     graphene_triangle_get_barycoords

(test graphene-triangle-barycoords
  (graphene:with-point3ds ((p1 0 0 0) (p2 0 2 0) (p3 1 1 0))
    (graphene:with-point3d (point 1 1 0)
      (graphene:with-vec2 (result)
        (graphene:with-triangle (triangle p1 p2 p3)
          (is (cffi:pointer-eq
                  result
                  (graphene:triangle-barycoords triangle point result)))
          (is (equal '(1.0 0.0)
                     (graphene:vec2-to-float result)))
          (graphene:triangle-barycoords triangle
                                        (graphene:point3d-init point 0 2 0)
                                        result)
          (is (equal '(0.0 1.0)
                     (graphene:vec2-to-float result))))))))

;;;     graphene_triangle_get_uv

;;;     graphene_triangle_contains_point

(test graphene-triangle-contains-point
  (graphene:with-point3ds ((p1 0 0 0) (p2 0 2 0) (p3 1 1 0))
    (graphene:with-point3ds ((point1 1/2 1 0) (point2 2 1 0))
      (graphene:with-triangle (triangle p1 p2 p3)
        (is-true (graphene:triangle-contains-point triangle point1))
        (is-false (graphene:triangle-contains-point triangle point2))))))

;;;     graphene_triangle_equal

(test graphene-triangle-equal
  (graphene:with-point3ds ((p1 0 0 0) (p2 0 2 0) (p3 1 1 0))
    (graphene:with-triangles ((triangle1 p1 p1 p1)
                              (triangle2 p1 p2 p3) (triangle3 p1 p2 p3))
      (is-false (graphene:triangle-equal triangle1 triangle2))
      (is-false (graphene:triangle-equal triangle1 triangle3))
      (is-true  (graphene:triangle-equal triangle2 triangle3)))))

;;; 2025-4-5
