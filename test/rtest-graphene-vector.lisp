(in-package :graphene-test)

(def-suite graphene-vector :in graphene-suite)
(in-suite graphene-vector)

;;; --- Types and Values -------------------------------------------------------

;;;     graphene_vec2_t
;;;     graphene_vec3_t
;;;     graphene_vec4_t

;;; --- Macros -----------------------------------------------------------------

(test with-vec2
  (graphene:with-vec2s (v1 (v2 1 2) (v3 v2) (v4 (v3 graphene:vec2-t)))
    (is (= 0.0 (graphene:vec2-x v1)))
    (is (= 0.0 (graphene:vec2-y v1)))

    (is (= 1.0 (graphene:vec2-x v2)))
    (is (= 2.0 (graphene:vec2-y v2)))

    (is (= 1.0 (graphene:vec2-x v3)))
    (is (= 2.0 (graphene:vec2-y v3)))

    (is (= 1.0 (graphene:vec2-x v4)))
    (is (= 2.0 (graphene:vec2-y v4)))))

(test with-vec3
  (graphene:with-vec3s (v1 (v2 1 2 3) (v3 v2) (v4 (v3 graphene:vec3-t)))
    (is (= 0.0 (graphene:vec3-x v1)))
    (is (= 0.0 (graphene:vec3-y v1)))
    (is (= 0.0 (graphene:vec3-z v1)))

    (is (= 1.0 (graphene:vec3-x v2)))
    (is (= 2.0 (graphene:vec3-y v2)))
    (is (= 3.0 (graphene:vec3-z v2)))

    (is (= 1.0 (graphene:vec3-x v3)))
    (is (= 2.0 (graphene:vec3-y v3)))
    (is (= 3.0 (graphene:vec3-z v3)))

    (is (= 1.0 (graphene:vec3-x v4)))
    (is (= 2.0 (graphene:vec3-y v4)))
    (is (= 3.0 (graphene:vec3-z v4)))))

(test with-vec4.1
  (graphene:with-vec4s (v1 (v2 1 2 3 4) (v3 v2) (v4 (v3 graphene:vec4-t)))
    (is (= 0.0 (graphene:vec4-x v1)))
    (is (= 0.0 (graphene:vec4-y v1)))
    (is (= 0.0 (graphene:vec4-z v1)))
    (is (= 0.0 (graphene:vec4-w v1)))

    (is (= 1.0 (graphene:vec4-x v2)))
    (is (= 2.0 (graphene:vec4-y v2)))
    (is (= 3.0 (graphene:vec4-z v2)))
    (is (= 4.0 (graphene:vec4-w v2)))

    (is (= 1.0 (graphene:vec4-x v4)))
    (is (= 2.0 (graphene:vec4-y v4)))
    (is (= 3.0 (graphene:vec4-z v4)))
    (is (= 4.0 (graphene:vec4-w v4)))

    (is (= 1.0 (graphene:vec4-x v4)))
    (is (= 2.0 (graphene:vec4-y v4)))
    (is (= 3.0 (graphene:vec4-z v4)))
    (is (= 4.0 (graphene:vec4-w v4)))))

(test with-vec4.2
  (graphene:with-vec3 (v 1 2 3)
    (graphene:with-vec4s ((v1 v 4) (v2 (v graphene:vec3-t) 4))
    (is (= 1.0 (graphene:vec4-x v1)))
    (is (= 2.0 (graphene:vec4-y v1)))
    (is (= 3.0 (graphene:vec4-z v1)))
    (is (= 4.0 (graphene:vec4-w v1)))

    (is (= 1.0 (graphene:vec4-x v2)))
    (is (= 2.0 (graphene:vec4-y v2)))
    (is (= 3.0 (graphene:vec4-z v2)))
    (is (= 4.0 (graphene:vec4-w v2))))))

(test with-vec4.3
  (graphene:with-vec2 (v 1 2)
    (graphene:with-vec4s ((v1 v 3 4) (v2 (v graphene:vec2-t) 3 4))
    (is (= 1.0 (graphene:vec4-x v1)))
    (is (= 2.0 (graphene:vec4-y v1)))
    (is (= 3.0 (graphene:vec4-z v1)))
    (is (= 4.0 (graphene:vec4-w v1)))

    (is (= 1.0 (graphene:vec4-x v2)))
    (is (= 2.0 (graphene:vec4-y v2)))
    (is (= 3.0 (graphene:vec4-z v2)))
    (is (= 4.0 (graphene:vec4-w v2))))))

;;; --- Functions --------------------------------------------------------------

;;;     graphene_vec2_alloc
;;;     graphene_vec2_free

(test vec2-alloc
  (let ((vector nil))
    (is (cffi:pointerp (setf vector (graphene:vec2-alloc))))
    (is-false (graphene:vec2-free vector))))

;;;     graphene_vec2_init

(test vec2-init.1
  (graphene:with-vec2 (vector)
    (is (cffi:pointerp (setf vector (graphene:vec2-init vector 1.0 2.0))))
    (is (= 1.0 (graphene:vec2-x vector)))
    (is (= 2.0 (graphene:vec2-y vector)))
    (is (cffi:pointerp (setf vector (graphene:vec2-init vector 1 1/2))))
    (is (= 1.0 (graphene:vec2-x vector)))
    (is (= 0.5 (graphene:vec2-y vector)))
    (is (cffi:pointerp (setf vector (graphene:vec2-init vector 2.5d0 3.5d0))))
    (is (= 2.5 (graphene:vec2-x vector)))
    (is (= 3.5 (graphene:vec2-y vector)))))

(test vec2-init.2
  (graphene:with-vec2 (vector 1.0 2.0)
    (is (= 1.0 (graphene:vec2-x vector)))
    (is (= 2.0 (graphene:vec2-y vector)))))

(test vec2-init.3
  (graphene:with-vec2s ((vector1 1.0 2.0) vector2)
    (is (= 1.0 (graphene:vec2-x vector1)))
    (is (= 2.0 (graphene:vec2-y vector1)))
    (is (= 0.0 (graphene:vec2-x vector2)))
    (is (= 0.0 (graphene:vec2-y vector2)))))

;;;     graphene_vec2_init_from_vec2

(test vec2-init-from-vec2
  (graphene:with-vec2s ((v1 1 2) v)
    (is (cffi:pointerp (setf v (graphene:vec2-init-from-vec2 v v1))))
    (is (= 1.0 (graphene:vec2-x v1)))
    (is (= 2.0 (graphene:vec2-y v1)))))

;;;     graphene_vec2_init_from_float

(test vec2-init-from-float.1
  (graphene:with-vec2 (v)
    (is (cffi:pointerp (setf v (graphene:vec2-init-from-float v '(1 2)))))
    (is (= 1.0 (graphene:vec2-x v)))
    (is (= 2.0 (graphene:vec2-y v)))))

(test vec2-init-from-float.2
  (let ((a 1) (b 2))
    (graphene:with-vec2 (v)
      (is (cffi:pointerp (setf v (graphene:vec2-init-from-float v (list a b)))))
      (is (= 1.0 (graphene:vec2-x v)))
      (is (= 2.0 (graphene:vec2-y v))))))

;;;     graphene_vec2_to_float

(test vec2-to-float
  (graphene:with-vec2s ((v 1 2) v1)
    (is (equal '(1.0 2.0) (graphene:vec2-to-float v)))
    (is (cffi:pointerp
            (setf v1
                  (graphene:vec2-init-from-float v1
                                                 (graphene:vec2-to-float v)))))
      (is (= 1.0 (graphene:vec2-x v1)))
      (is (= 2.0 (graphene:vec2-y v1)))))

;;;     graphene_vec2_add

(test vec2-add
  (graphene:with-vec2s ((a 1 2) (b 3 4) result)
    (is (equal '(4.0 6.0)
               (graphene:vec2-to-float (graphene:vec2-add a b result))))))

;;;     graphene_vec2_subtract

(test vec2-subtract
  (graphene:with-vec2s ((a 1 2) (b 3 4) result)
    (is (equal '(2.0 2.0)
               (graphene:vec2-to-float (graphene:vec2-subtract b a result))))))

;;;     graphene_vec2_multiply

(test vec2-multiply
  (graphene:with-vec2s ((a 1 2) (b 3 4) result)
    (is (equal '(3.0 8.0)
               (graphene:vec2-to-float (graphene:vec2-multiply a b result))))))

;;;     graphene_vec2_divide

(test vec2-divide
  (graphene:with-vec2s ((a 2.0 2.0) (b 3.0 4.0) result)
    (is (equal '(2.0 2.0) (graphene:vec2-to-float a)))
    (is (equal '(3.0 4.0) (graphene:vec2-to-float b)))
    (is (every #'approx-equal '(1.5 2.0)
                               (graphene:vec2-to-float (graphene:vec2-divide b a result))))))

;;;     graphene_vec2_dot

(test vec2-dot
  (graphene:with-vec2s ((v1 1 2) (v2 3 4))
    (is (= 11.0 (graphene:vec2-dot v1 v2)))))

;;;     graphene_vec2_scale

(test vec2-scale
  (graphene:with-vec2s ((v 1 2) result)
    (is (equal '(2.0 4.0)
               (graphene:vec2-to-float (graphene:vec2-scale v 2 result))))
    (is (equal '(0.5 1.0)
               (graphene:vec2-to-float (graphene:vec2-scale v 1/2 result))))))

;;;     graphene_vec2_length

(test vec2-length
  (graphene:with-vec2s ((v1 1 2) (v2 3 4))
    (is (= (sqrt  5) (graphene:vec2-length v1)))
    (is (= (sqrt 25) (graphene:vec2-length v2)))))

;;;     graphene_vec2_normalize

(test vec2-normalize
  (graphene:with-vec2s ((v1 4 0) (v2 0 5) result)
    (is (cffi:pointerp (setf result (graphene:vec2-normalize v1 result))))
    (is (approx-equal 1.0 (graphene:vec2-length result)))
    (is (cffi:pointerp (setf result (graphene:vec2-normalize v2 result))))
    (is (approx-equal 1.0 (graphene:vec2-length result)))))

;;;     graphene_vec2_negate
;;;     graphene_vec2_equal
;;;     graphene_vec2_near
;;;     graphene_vec2_min
;;;     graphene_vec2_max
;;;     graphene_vec2_interpolate
;;;     graphene_vec2_get_x
;;;     graphene_vec2_get_y
;;;     graphene_vec2_zero
;;;     graphene_vec2_one
;;;     graphene_vec2_x_axis
;;;     graphene_vec2_y_axis
;;;
;;;     graphene_vec3_alloc
;;;     graphene_vec3_free
;;;     graphene_vec3_init

;;;     graphene_vec3_init_from_vec3

(test vec3-init-from-vec3
  (graphene:with-vec3s ((v1 1 2 3) v)
    (is (cffi:pointerp (setf v (graphene:vec3-init-from-vec3 v v1))))
    (is (= 1.0 (graphene:vec3-x v1)))
    (is (= 2.0 (graphene:vec3-y v1)))
    (is (= 3.0 (graphene:vec3-z v1)))))

;;;     graphene_vec3_init_from_float

(test vec3-init-from-float.1
  (graphene:with-vec3 (v)
    (is (cffi:pointerp (setf v (graphene:vec3-init-from-float v '(1 2 3)))))
    (is (= 1.0 (graphene:vec3-x v)))
    (is (= 2.0 (graphene:vec3-y v)))
    (is (= 3.0 (graphene:vec3-z v)))))

(test vec3-init-from-float.2
  (let ((a 1) (b 2) (c 3))
    (graphene:with-vec3 (v)
      (is (cffi:pointerp (setf v (graphene:vec3-init-from-float v (list a b c)))))
      (is (= 1.0 (graphene:vec3-x v)))
      (is (= 2.0 (graphene:vec3-y v)))
      (is (= 3.0 (graphene:vec3-z v))))))

;;;     graphene_vec3_to_float

(test vec3-to-float
  (graphene:with-vec3s ((v 1 2 3) v1)
    (is (equal '(1.0 2.0 3.0) (graphene:vec3-to-float v)))
    (is (cffi:pointerp (setf v1 (graphene:vec3-init-from-float v1 (graphene:vec3-to-float v)))))
    (is (= 1.0 (graphene:vec3-x v1)))
    (is (= 2.0 (graphene:vec3-y v1)))
    (is (= 3.0 (graphene:vec3-z v1)))))

;;;     graphene_vec3_add

(test vec3-add
  (graphene:with-vec3s ((a 1 2 3) (b 3 4 5) result)
    (is (equal '(4.0 6.0 8.0)
               (graphene:vec3-to-float (graphene:vec3-add a b result))))))

;;;     graphene_vec3_subtract

(test vec3-subtract
  (graphene:with-vec3s ((a 1 2 3) (b 3 4 5) result)
    (is (equal '(2.0 2.0 2.0)
               (graphene:vec3-to-float (graphene:vec3-subtract b a result))))))

;;;     graphene_vec3_multiply

(test vec3-multiply
  (graphene:with-vec3s ((a 1 2 3) (b 3 4 5) result)
    (is (equal '(3.0 8.0 15.0)
               (graphene:vec3-to-float (graphene:vec3-multiply a b result))))))

;;;     graphene_vec3_divide

(test vec3-divide
  (graphene:with-vec3s ((a 2.0 2.0 2.0) (b 3.0 4.0 5.0) result)
    (is (equal '(2.0 2.0 2.0) (graphene:vec3-to-float a)))
    (is (equal '(3.0 4.0 5.0) (graphene:vec3-to-float b)))
    ;; FIXME: Signals an error
    ;;   VEC3-DIVIDE in GRAPHENE-VECTOR []:
    ;;   Unexpected Error: #<FLOATING-POINT-INVALID-OPERATION {100278A543}>
    ;;   arithmetic error FLOATING-POINT-INVALID-OPERATION signalled.
    (is (every #'approx-equal '(1.5 2.0 2.5)
                              (graphene:vec3-to-float (graphene:vec3-divide b a result))))))

;;;     graphene_vec3_cross

(test vec3-cross
  (graphene:with-vec3s ((v1 1 0 0) (v2 0 1 0) (v3 0 0 1) result)
    (is (cffi:pointerp (setf result (graphene:vec3-cross v1 v2 result))))
    (is (=  1.0 (graphene:vec3-x v1)))
    (is (=  0.0 (graphene:vec3-y v1)))
    (is (=  0.0 (graphene:vec3-z v1)))

    (is (=  0.0 (graphene:vec3-x v2)))
    (is (=  1.0 (graphene:vec3-y v2)))
    (is (=  0.0 (graphene:vec3-z v2)))

    (is (=  0.0 (graphene:vec3-x result)))
    (is (=  0.0 (graphene:vec3-y result)))
    (is (=  1.0 (graphene:vec3-z result)))

    (is (cffi:pointerp (setf result (graphene:vec3-cross v1 v3 result))))

    (is (=  0.0 (graphene:vec3-x result)))
    (is (= -1.0 (graphene:vec3-y result)))
    (is (=  0.0 (graphene:vec3-z result)))

    (is (cffi:pointerp (setf result (graphene:vec3-cross v2 v3 result))))

    (is (=  1.0 (graphene:vec3-x result)))
    (is (=  0.0 (graphene:vec3-y result)))
    (is (=  0.0 (graphene:vec3-z result)))
))

;;;     graphene_vec3_dot

(test vec3-dot
  (graphene:with-vec3s ((v1 1 2 3) (v2 3 4 5))
    (is (= 26.0 (graphene:vec3-dot v1 v2)))))

;;;     graphene_vec3_scale

(test vec3-scale
  (graphene:with-vec3s ((v 1 2 3) result)
    (is (equal '(2.0 4.0 6.0)
               (graphene:vec3-to-float (graphene:vec3-scale v 2 result))))
    (is (equal '(0.5 1.0 1.5)
               (graphene:vec3-to-float (graphene:vec3-scale v 1/2 result))))))

;;;     graphene_vec3_length

(test vec3-length
  (graphene:with-vec3s ((v1 1 2 0) (v2 3 0 4))
    (is (= (sqrt  5) (graphene:vec3-length v1)))
    (is (= (sqrt 25) (graphene:vec3-length v2)))))

;;;     graphene_vec3_normalize

(test vec3-normalize
  (graphene:with-vec3s ((v1 4 0 0) (v2 0 5 0) (v3 0 0 6) result)
    (is (cffi:pointerp (setf result (graphene:vec3-normalize v1 result))))
    (is (approx-equal 1.0 (graphene:vec3-length result)))
    (is (cffi:pointerp (setf result (graphene:vec3-normalize v2 result))))
    (is (approx-equal 1.0 (graphene:vec3-length result)))
    (is (cffi:pointerp (setf result (graphene:vec3-normalize v3 result))))
    (is (approx-equal 1.0 (graphene:vec3-length result)))))

;;;     graphene_vec3_negate
;;;     graphene_vec3_equal
;;;     graphene_vec3_near
;;;     graphene_vec3_min
;;;     graphene_vec3_max
;;;     graphene_vec3_interpolate
;;;     graphene_vec3_get_x
;;;     graphene_vec3_get_y
;;;     graphene_vec3_get_z
;;;     graphene_vec3_get_xy
;;;     graphene_vec3_get_xy0
;;;     graphene_vec3_get_xyz0
;;;     graphene_vec3_get_xyz1
;;;     graphene_vec3_get_xyzw
;;;     graphene_vec3_zero
;;;     graphene_vec3_one
;;;     graphene_vec3_x_axis
;;;     graphene_vec3_y_axis
;;;     graphene_vec3_z_axis
;;;
;;;     graphene_vec4_alloc
;;;     graphene_vec4_free
;;;     graphene_vec4_init

;;;     graphene_vec4_init_from_vec4

(test vec4-init-from-vec4
  (graphene:with-vec4s ((v1 1 2 3 4) v)
    (is (cffi:pointerp (setf v (graphene:vec4-init-from-vec4 v v1))))
    (is (= 1.0 (graphene:vec4-x v1)))
    (is (= 2.0 (graphene:vec4-y v1)))
    (is (= 3.0 (graphene:vec4-z v1)))
    (is (= 4.0 (graphene:vec4-w v1)))))

;;;     graphene_vec4_init_from_vec3

(test vec4-init-from-vec3
  (let ((w 4))
    (graphene:with-vec3 (v1 1 2 3)
      (graphene:with-vec4 (v)
        (is (cffi:pointerp (setf v (graphene:vec4-init-from-vec3 v v1 w))))
        (is (equal '(1.0 2.0 3.0 4.0) (graphene:vec4-to-float v)))))))

;;;     graphene_vec4_init_from_vec2

(test vec4-init-from-vec2
  (let ((z 3) (w 4))
    (graphene:with-vec2 (v1 1 2)
      (graphene:with-vec4 (v)
        (is (cffi:pointerp (setf v (graphene:vec4-init-from-vec2 v v1 z w))))
        (is (equal '(1.0 2.0 3.0 4.0) (graphene:vec4-to-float v)))))))

;;;     graphene_vec4_init_from_float

(test vec4-init-from-float.1
  (graphene:with-vec4 (v)
    (is (cffi:pointerp (setf v (graphene:vec4-init-from-float v '(1 2 3 4)))))
    (is (= 1.0 (graphene:vec4-x v)))
    (is (= 2.0 (graphene:vec4-y v)))
    (is (= 3.0 (graphene:vec4-z v)))
    (is (= 4.0 (graphene:vec4-w v)))))

(test vec4-init-from-float.2
  (let ((a 1) (b 2) (c 3) (d 4))
    (graphene:with-vec4 (v)
      (is (cffi:pointerp (setf v (graphene:vec4-init-from-float v (list a b c d)))))
    (is (= 1.0 (graphene:vec4-x v)))
    (is (= 2.0 (graphene:vec4-y v)))
    (is (= 3.0 (graphene:vec4-z v)))
    (is (= 4.0 (graphene:vec4-w v))))))

;;;     graphene_vec4_to_float

(test vec4-to-float
  (graphene:with-vec4s ((v 1 2 3 4) v1)
    (is (equal '(1.0 2.0 3.0 4.0) (graphene:vec4-to-float v)))
    (is (cffi:pointerp (setf v1 (graphene:vec4-init-from-float v1 (graphene:vec4-to-float v)))))
    (is (= 1.0 (graphene:vec4-x v1)))
    (is (= 2.0 (graphene:vec4-y v1)))
    (is (= 3.0 (graphene:vec4-z v1)))
    (is (= 4.0 (graphene:vec4-w v1)))))

;;;     graphene_vec4_add

(test vec4-subtract
  (graphene:with-vec4s ((a 1 2 3 4) (b 3 4 5 6) result)
    (is (equal '(4.0 6.0 8.0 10.0)
               (graphene:vec4-to-float (graphene:vec4-add a b result))))))

;;;     graphene_vec4_subtract

(test vec4-subtract
  (graphene:with-vec4s ((a 1 2 3 4) (b 3 4 5 6) result)
    (is (equal '(2.0 2.0 2.0 2.0)
               (graphene:vec4-to-float (graphene:vec4-subtract b a result))))))

;;;     graphene_vec4_multiply

(test vec4-multiply
  (graphene:with-vec4s ((a 1 2 3 4) (b 3 4 5 6) result)
    (is (equal '(3.0 8.0 15.0 24.0)
               (graphene:vec4-to-float (graphene:vec4-multiply a b result))))))

;;;     graphene_vec4_divide

(test vec4-divide
  (graphene:with-vec4s ((a 1.0 1.0 1.0 0.5) (b 3.0 4.0 5.0 1.0) result)
    (is (equal '(1.0 1.0 1.0 0.5) (graphene:vec4-to-float a)))
    (is (equal '(3.0 4.0 5.0 1.0) (graphene:vec4-to-float b)))
    (is (every #'approx-equal
               '(3.0 4.0 5.0 2.0)
               (graphene:vec4-to-float (graphene:vec4-divide b a result))))
))

;;;     graphene_vec4_dot

(test vec4-dot
  (graphene:with-vec4s ((v1 1 2 3 4) (v2 3 4 5 6))
    (is (= 50.0 (graphene:vec4-dot v1 v2)))))

;;;     graphene_vec4_scale

(test vec4-scale
  (graphene:with-vec4s ((v 1 2 3 4) result)
    (is (equal '(2.0 4.0 6.0 8.0)
               (graphene:vec4-to-float (graphene:vec4-scale v 2 result))))
    (is (equal '(0.5 1.0 1.5 2.0)
               (graphene:vec4-to-float (graphene:vec4-scale v 1/2 result))))))

;;;     graphene_vec4_length

(test vec4-length
  (graphene:with-vec4s ((v1 1 2 0 0) (v2 3 0 0 4))
    (is (= (sqrt  5) (graphene:vec4-length v1)))
    (is (= (sqrt 25) (graphene:vec4-length v2)))))

;;;     graphene_vec4_normalize

(test vec4-normalize
  (graphene:with-vec4s ((v1 4 0 0 0) (v2 0 5 0 0) (v3 0 0 6 0) (v4 0 0 0 7) result)
    (is (cffi:pointerp (setf result (graphene:vec4-normalize v1 result))))
    (is (approx-equal 1.0 (graphene:vec4-length result)))
    (is (cffi:pointerp (setf result (graphene:vec4-normalize v2 result))))
    (is (approx-equal 1.0 (graphene:vec4-length result)))
    (is (cffi:pointerp (setf result (graphene:vec4-normalize v3 result))))
    (is (approx-equal 1.0 (graphene:vec4-length result)))
    (is (cffi:pointerp (setf result (graphene:vec4-normalize v4 result))))
    (is (approx-equal 1.0 (graphene:vec4-length result)))))

;;;     graphene_vec4_negate
;;;     graphene_vec4_equal
;;;     graphene_vec4_near
;;;     graphene_vec4_min
;;;     graphene_vec4_max
;;;     graphene_vec4_interpolate

;;;     graphene_vec4_get_x
;;;     graphene_vec4_get_y
;;;     graphene_vec4_get_z
;;;     graphene_vec4_get_w

(test vec4-x/y/z/w
  (graphene:with-vec4 (v 1 2 3 4)
    (is (= 1.0 (graphene:vec4-x v)))
    (is (= 2.0 (graphene:vec4-y v)))
    (is (= 3.0 (graphene:vec4-z v)))
    (is (= 4.0 (graphene:vec4-w v)))))

;;;     graphene_vec4_get_xy

(test vec4-xy
  (graphene:with-vec2 (result)
    (graphene:with-vec4 (v 1 2 3 4)
      (is (equal '(1.0 2.0)
                 (graphene:vec2-to-float (graphene:vec4-xy v result)))))))

;;;     graphene_vec4_get_xyz

(test vec4-xyz
  (graphene:with-vec3 (result)
    (graphene:with-vec4 (v 1 2 3 4)
      (is (equal '(1.0 2.0 3.0)
                 (graphene:vec3-to-float (graphene:vec4-xyz v result)))))))

;;;     graphene_vec4_zero
;;;     graphene_vec4_one
;;;     graphene_vec4_x_axis
;;;     graphene_vec4_y_axis
;;;     graphene_vec4_z_axis
;;;     graphene_vec4_w_axis

;;; 2022-12-3

