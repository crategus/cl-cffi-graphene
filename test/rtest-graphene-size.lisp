(in-package :graphene-test)

(def-suite graphene-size :in graphene-suite)
(in-suite graphene-size)

;;; --- Types and Values -------------------------------------------------------

;;;     graphene_size_t

;;; --- Macros -----------------------------------------------------------------

;;;     with-graphene-size

(test with-graphene-size.1
  (with-graphene-size (s)
    (is (= 0.0 (size-width s)))
    (is (= 0.0 (size-height s)))))

(test with-graphene-size.2
  (with-graphene-size (s 1 2)
    (is (= 1.0 (size-width s)))
    (is (= 2.0 (size-height s)))))

(test with-graphene-size.3
  (with-graphene-sizes ((s1 3 4) (s s1))
    (is (= 3.0 (size-width s)))
    (is (= 4.0 (size-height s)))))

(test with-graphene-size.4
  (with-graphene-sizes ((s1 5 6) (s (s1 size-t)))
      (is (= 5.0 (size-width s)))
      (is (= 6.0 (size-height s)))))

;;     with-graphene-sizes

(test with-graphene-sizes
  (with-graphene-sizes (s1 (s2 1.5 2.5) (s3 s2) (s4 (s2 size-t)))
    (is (= 0.0 (size-width s1)))
    (is (= 0.0 (size-height s1)))
    (is (= 1.5 (size-width s2)))
    (is (= 2.5 (size-height s2)))
    (is (= 1.5 (size-width s3)))
    (is (= 2.5 (size-height s3)))
    (is (= 1.5 (size-width s4)))
    (is (= 2.5 (size-height s4)))))

;;; --- Functions --------------------------------------------------------------

;;;     size-width
;;;     size-height

(test size-width/height
  (with-graphene-size (s)
    (is (= 0.0 (size-width s)))
    (is (= 0.0 (size-height s)))
    (is (= 1.0 (setf (size-width s) 1.0)))
    (is (= 2.0 (setf (size-height s) 2.0)))
    (is (= 1.0 (size-width s)))
    (is (= 2.0 (size-height s)))))

;;;     graphene_size_alloc
;;;     graphene_size_free

(test size-alloc/free
  (let ((size nil))
    (is (pointerp (setf size (size-alloc))))
    (is (= 0.0 (size-height size)))
    (is (= 0.0 (size-width size)))
    (is-false (size-free size))))

;;;     graphene_size_zero

(test size-zero
  (let ((size (size-zero)))
    (is (= 0.0 (size-width size)))
    (is (= 0.0 (size-height size)))))

;;;     graphene_size_init

(test size-init.1
  (with-graphene-size (size)
    (is (size-equal size (size-zero)))
    (is (pointer-eq size (size-init size 1.0 2.0)))
    (is (= 1.0 (size-width size)))
    (is (= 2.0 (size-height size)))
    (is (pointer-eq size (size-init size 1 1/2)))
    (is (= 1.0 (size-width size)))
    (is (= 0.5 (size-height size)))
    (is (pointer-eq size (size-init size 2.5d0 3.5d0)))
    (is (= 2.5 (size-width size)))
    (is (= 3.5 (size-height size)))))

(test size-init.2
  (with-graphene-size (size 1.0 2.0)
    (is (= 1.0 (size-width size)))
    (is (= 2.0 (size-height size)))))

(test size-init.3
  (with-graphene-points ((size1 1.0 2.0) size2)
    (is (= 1.0 (size-width size1)))
    (is (= 2.0 (size-height size1)))
    (is (size-equal size2 (size-zero)))))

;;;     graphene_size_init_from_size

(test size-init-from-size
  (with-graphene-sizes ((size1 1.0 2.0) size2)
    (is (pointer-eq size2 (size-init-from-size size2 size1)))
    (is (= 1.0 (size-width size2)))
    (is (= 2.0 (size-height size2)))))

;;;     graphene_size_equal

(test size-equal
  (with-graphene-sizes ((size1 1.0 2.0) (size2 1.0 2.0) (size3 0 0))
    (is-true (size-equal size1 size2))
    (is-false (size-equal size1 size3))
    (is-false (size-equal size2 size3))))

;;;     graphene_size_interpolate

(test size-interpolate
  (with-graphene-sizes ((size1 0.0 0.0)
                        (size2 1.0 0.0)
                        (size3 0.0 1.0)
                        result)
    (is (pointer-eq result (size-interpolate size1 size2 0.1 result)))
    (is (= 0.1 (size-width result)))
    (is (= 0.0 (size-height result)))
    (is (pointer-eq result (size-interpolate size1 size3 0.1 result)))
    (is (= 0.0 (size-width result)))
    (is (= 0.1 (size-height result)))))

;;;     graphene_size_scale

(test size-scale
  (with-graphene-size (size 1.0 2.0)
    (is (pointer-eq size (size-scale size 2.0 size)))
    (is (= 2.0 (size-width size)))
    (is (= 4.0 (size-height size)))
    (is (pointer-eq size (size-scale size 1/2 size)))
    (is (= 1.0 (size-width size)))
    (is (= 2.0 (size-height size)))))

;;; 2022-10-1
