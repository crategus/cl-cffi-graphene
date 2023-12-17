(in-package :graphene-test)

(def-suite graphene-size :in graphene-suite)
(in-suite graphene-size)

;;; --- Types and Values -------------------------------------------------------

;;;     graphene_size_t

(test graphene-size-t-structure
  (is (= 8 (cffi:foreign-type-size '(:struct graphene:size-t))))
  (is (equal '(graphene::width graphene::height)
             (cffi:foreign-slot-names '(:struct graphene:size-t))))
  (cffi:with-foreign-object (size '(:struct graphene:size-t))
    (is (= 0 (setf (graphene:size-width size) 0)))
    (is (= 0 (graphene:size-width size)))
    (is (= 0 (setf (graphene:size-height size) 0)))
    (is (= 0 (graphene:size-height size)))))

;;; --- Macros -----------------------------------------------------------------

;;;     with-size

(test graphene-with-size.1
  (graphene:with-size (s)
    (is (= 0.0 (graphene:size-width s)))
    (is (= 0.0 (graphene:size-height s)))))

(test graphene-with-size.2
  (graphene:with-size (s 1 2)
    (is (= 1.0 (graphene:size-width s)))
    (is (= 2.0 (graphene:size-height s)))))

(test graphene-with-size.3
  (graphene:with-sizes ((s1 3 4) (s s1))
    (is (= 3.0 (graphene:size-width s)))
    (is (= 4.0 (graphene:size-height s)))))

(test graphene-with-size.4
  (graphene:with-sizes ((s1 5 6) (s (s1 graphene:size-t)))
      (is (= 5.0 (graphene:size-width s)))
      (is (= 6.0 (graphene:size-height s)))))

;;     with-sizes

(test graphene-with-sizes
  (graphene:with-sizes (s1 (s2 1.5 2.5)
                           (s3 s2)
                           (s4 (s2 graphene:size-t)))
    (is (= 0.0 (graphene:size-width s1)))
    (is (= 0.0 (graphene:size-height s1)))
    (is (= 1.5 (graphene:size-width s2)))
    (is (= 2.5 (graphene:size-height s2)))
    (is (= 1.5 (graphene:size-width s3)))
    (is (= 2.5 (graphene:size-height s3)))
    (is (= 1.5 (graphene:size-width s4)))
    (is (= 2.5 (graphene:size-height s4)))))

;;; --- Functions --------------------------------------------------------------

;;;     size-width
;;;     size-height

(test graphene-size-width/height
  (graphene:with-size (s)
    (is (= 0.0 (graphene:size-width s)))
    (is (= 0.0 (graphene:size-height s)))
    (is (= 1.0 (setf (graphene:size-width s) 1.0)))
    (is (= 2.0 (setf (graphene:size-height s) 2.0)))
    (is (= 1.0 (graphene:size-width s)))
    (is (= 2.0 (graphene:size-height s)))))

;;;     graphene_size_alloc
;;;     graphene_size_free

(test graphene-size-alloc/free
  (let ((size nil))
    (is (cffi:pointerp (setf size (graphene:size-alloc))))
    (is (= 0.0 (graphene:size-height size)))
    (is (= 0.0 (graphene:size-width size)))
    (is-false (graphene:size-free size))))

;;;     graphene_size_zero

(test graphene-size-zero
  (let ((size (graphene:size-zero)))
    (is (= 0.0 (graphene:size-width size)))
    (is (= 0.0 (graphene:size-height size)))))

;;;     graphene_size_init

(test graphene-size-init.1
  (graphene:with-size (size)
    (is (graphene:size-equal size (graphene:size-zero)))
    (is (cffi:pointer-eq size (graphene:size-init size 1.0 2.0)))
    (is (= 1.0 (graphene:size-width size)))
    (is (= 2.0 (graphene:size-height size)))
    (is (cffi:pointer-eq size (graphene:size-init size 1 1/2)))
    (is (= 1.0 (graphene:size-width size)))
    (is (= 0.5 (graphene:size-height size)))
    (is (cffi:pointer-eq size (graphene:size-init size 2.5d0 3.5d0)))
    (is (= 2.5 (graphene:size-width size)))
    (is (= 3.5 (graphene:size-height size)))))

(test graphene-size-init.2
  (graphene:with-size (size 1.0 2.0)
    (is (= 1.0 (graphene:size-width size)))
    (is (= 2.0 (graphene:size-height size)))))

(test graphene-size-init.3
  (graphene:with-points ((size1 1.0 2.0) size2)
    (is (= 1.0 (graphene:size-width size1)))
    (is (= 2.0 (graphene:size-height size1)))
    (is (graphene:size-equal size2 (graphene:size-zero)))))

;;;     graphene_size_init_from_size

(test graphene-size-init-from-size
  (graphene:with-sizes ((size1 1.0 2.0) size2)
    (is (cffi:pointer-eq size2 (graphene:size-init-from-size size2 size1)))
    (is (= 1.0 (graphene:size-width size2)))
    (is (= 2.0 (graphene:size-height size2)))))

;;;     graphene_size_equal

(test graphene-size-equal
  (graphene:with-sizes ((size1 1.0 2.0) (size2 1.0 2.0) (size3 0 0))
    (is-true (graphene:size-equal size1 size2))
    (is-false (graphene:size-equal size1 size3))
    (is-false (graphene:size-equal size2 size3))))

;;;     graphene_size_interpolate

(test graphene-size-interpolate
  (graphene:with-sizes ((size1 0.0 0.0)
                        (size2 1.0 0.0)
                        (size3 0.0 1.0)
                        result)
    (is (cffi:pointer-eq result
            (graphene:size-interpolate size1 size2 0.1 result)))
    (is (= 0.1 (graphene:size-width result)))
    (is (= 0.0 (graphene:size-height result)))
    (is (cffi:pointer-eq result
            (graphene:size-interpolate size1 size3 0.1 result)))
    (is (= 0.0 (graphene:size-width result)))
    (is (= 0.1 (graphene:size-height result)))))

;;;     graphene_size_scale

(test graphene-size-scale
  (graphene:with-size (size 1.0 2.0)
    (is (cffi:pointer-eq size (graphene:size-scale size 2.0 size)))
    (is (= 2.0 (graphene:size-width size)))
    (is (= 4.0 (graphene:size-height size)))
    (is (cffi:pointer-eq size (graphene:size-scale size 1/2 size)))
    (is (= 1.0 (graphene:size-width size)))
    (is (= 2.0 (graphene:size-height size)))))

;;; 2023-12-10
