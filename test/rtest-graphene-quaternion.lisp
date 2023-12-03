(in-package :graphene-test)

(def-suite graphene-quaternion :in graphene-suite)
(in-suite graphene-quaternion)

;;; --- Types and Values -------------------------------------------------------

;;;     graphene_quaternion_t

;;; --- Macros -----------------------------------------------------------------

(test with-quaternion.1
  (graphene:with-vec4 (vector)
    (graphene:with-quaternion (quaternion)
      (is (cffi:pointerp quaternion))
      (is (cffi:pointer-eq vector (graphene:quaternion-to-vec4 quaternion vector)))
      (is (equal '(0.0 0.0 0.0 0.0)
                 (graphene:vec4-to-float vector))))))

(test with-quaternion.2
  (graphene:with-vec4 (vector)
    (graphene:with-quaternion (quaternion 1 2 3 4)
      (is (cffi:pointerp quaternion))
      (is (cffi:pointer-eq vector (graphene:quaternion-to-vec4 quaternion vector)))
      (is (equal '(1.0 2.0 3.0 4.0)
                 (graphene:vec4-to-float vector))))))

(test with-quaternion.3
  (graphene:with-vec4 (vector)
    (graphene:with-quaternion (quaternion (5 :float) 6 7 8)
      (is (cffi:pointerp quaternion))
      (is (cffi:pointer-eq vector (graphene:quaternion-to-vec4 quaternion vector)))
      (is (equal '(5.0 6.0 7.0 8.0)
                 (graphene:vec4-to-float vector))))))

(test with-quaternion.4
  (graphene:with-vec4 (vector)
    (graphene:with-quaternion (quaternion (180 :deg) 0 0 0)
      (is (cffi:pointerp quaternion))
      (is (cffi:pointer-eq vector (graphene:quaternion-to-vec4 quaternion vector)))
      (is (every #'approx-equal '(1.0 0.0 0.0 0.0)
                                (graphene:vec4-to-float vector))))))

(test with-quaternion.5
  (graphene:with-vec4 (vector)
    (graphene:with-quaternion (quaternion (pi :rad) 0 0 0)
      (is (cffi:pointerp quaternion))
      (is (cffi:pointer-eq vector (graphene:quaternion-to-vec4 quaternion vector)))
      (is (every #'approx-equal '(1.0 0.0 0.0 0.0)
                                (graphene:vec4-to-float vector))))))

(test with-quaternion.6
  (graphene:with-vec4 (vector)
    (graphene:with-quaternions ((quaternion1 1 2 3 4) (quaternion quaternion1))
      (is (cffi:pointerp quaternion))
      (is (cffi:pointer-eq vector (graphene:quaternion-to-vec4 quaternion vector)))
      (is (every #'approx-equal '(1.0 2.0 3.0 4.0)
                                 (graphene:vec4-to-float vector))))))

(test with-quaternion.7
  (graphene:with-vec4s (vector (vector1 1 2 3 4))
    (graphene:with-quaternion (quaternion (vector1 graphene:vec4-t))
      (is (cffi:pointerp quaternion))
      (is (cffi:pointer-eq vector (graphene:quaternion-to-vec4 quaternion vector)))
      (is (every #'approx-equal '(1.0 2.0 3.0 4.0)
                                (graphene:vec4-to-float vector))))))

(test with-quaternion.8
  (graphene:with-vec4 (vector)
    (graphene:with-matrix (matrix)
      (is (cffi:pointer-eq matrix (graphene:matrix-init-identity matrix)))
      (graphene:with-quaternion (quaternion (matrix graphene:matrix-t))
        (is (cffi:pointerp quaternion))
        (is (cffi:pointer-eq vector (graphene:quaternion-to-vec4 quaternion vector)))
        (is (every #'approx-equal '(0.0 0.0 0.0 1.0)
                                  (graphene:vec4-to-float vector)))))))

(test with-quaternion.9
  (graphene:with-vec4 (vector)
    (graphene:with-euler (euler 0 0 0)
      (graphene:with-quaternion (quaternion (euler graphene:euler-t))
        (is (cffi:pointerp quaternion))
        (is (cffi:pointer-eq vector (graphene:quaternion-to-vec4 quaternion vector)))
        (is (every #'approx-equal '(0.0 0.0 0.0 1.0)
                                  (graphene:vec4-to-float vector)))))))

#+nil
(test with--quaternion.10
  (graphene:with-vec4 (vector)
    (graphene:with-vec3 (vec3 1 0 0)
      (graphene:with-quaternion (quaternion 180 vec3)
        (is (cffi:pointerp quaternion))
        (is (cffi:pointer-eq vector (graphene:quaternion-to-vec4 quaternion vector)))
        ;; TODO: Choose a simple example
        (is (every #'approx-equal '(0.43733734 0.39079875 -0.7797218 -0.2191947)
                                  (graphene:vec4-to-float vector)))))))

;;; --- Functions --------------------------------------------------------------

;;;     graphene_quaternion_alloc
;;;     graphene_quaternion_free
;;;     graphene_quaternion_init
;;;     graphene_quaternion_init_identity
;;;     graphene_quaternion_init_from_quaternion
;;;     graphene_quaternion_init_from_vec4
;;;     graphene_quaternion_init_from_matrix
;;;     graphene_quaternion_init_from_angles
;;;     graphene_quaternion_init_from_radians
;;;     graphene_quaternion_init_from_angle_vec3
;;;     graphene_quaternion_init_from_euler
;;;     graphene_quaternion_to_vec4
;;;     graphene_quaternion_to_matrix
;;;     graphene_quaternion_to_angles
;;;     graphene_quaternion_to_radians
;;;     graphene_quaternion_to_angle_vec3
;;;     graphene_quaternion_equal
;;;     graphene_quaternion_dot
;;;     graphene_quaternion_invert
;;;     graphene_quaternion_normalize
;;;     graphene_quaternion_add
;;;     graphene_quaternion_multiply
;;;     graphene_quaternion_scale
;;;     graphene_quaternion_slerp

;;; 2022-9-24