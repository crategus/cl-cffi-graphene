(in-package :graphene-test)

(def-suite graphene-frustum :in graphene-suite)
(in-suite graphene-frustum)



#+nil
(test array-ptr

  (with-graphene-vec3s ((v1 1 0 0)
                        (v2 0 1 0)
                        (v3 0 0 1)
                        (v4 1 1 0)
                        (v5 0 1 1)
                        (v6 1 1 1))

    (with-foreign-object (array-ptr :pointer 6)
      (loop for i from 0 below 6
            for val in (list v1 v2 v3 v4 v5 v6)
            do (setf (mem-aref array-ptr :pointer i) val))

;    (format t "~&array-ptr : ~a~%" array-ptr)
;    (format t "mem-aptr  : ~a~%" (mem-aptr array-ptr :pointer 0))
;    (format t "mem-aptr  : ~a~%" (mem-aptr array-ptr :pointer 1))


;    (format t "Read the array:~%")
;    (format t " 0 : ~a ~a~%" v1 (mem-aref array-ptr :pointer 0))
;    (format t " 1 : ~a ~a~%" v2 (mem-aref array-ptr :pointer 1))
;    (format t " 2 : ~a ~a~%" v3 (mem-aref array-ptr :pointer 2))
;    (format t " 3 : ~a ~a~%" v4 (mem-aref array-ptr :pointer 3))
;    (format t " 4 : ~a ~a~%" v5 (mem-aref array-ptr :pointer 4))
;    (format t " 5 : ~a ~a~%" v6 (mem-aref array-ptr :pointer 5))


)))

;;; --- Types and Values -------------------------------------------------------

;;;     graphene_frustum_t

;;; --- Functions --------------------------------------------------------------

;;;     graphene_frustum_alloc
;;;     graphene_frustum_free
;;;     graphene_frustum_init
;;;     graphene_frustum_init_from_frustum
;;;     graphene_frustum_init_from_matrix

;;;     graphene_frustum_get_planes

;; Causes an error:
;; CORRUPTION WARNING in SBCL pid 3837 tid 3837:
;; Memory fault at (nil) (pc=0x7fcf715b441a, fp=0x55753eca1320,
;; sp=0x7fcf71c2eb68) tid 3837

#+nil
(test frustum-planes
  (with-graphene-vec3s ((vec1 1 0 0)
                        (vec2 0 1 0)
                        (vec3 0 0 1)
                        (vec4 1 1 0)
                        (vec5 0 1 1)
                        (vec6 1 1 1))

    (with-graphene-planes ((plane1 vec1 1)
                           (plane2 vec2 2)
                           (plane3 vec3 3)
                           (plane4 vec4 4)
                           (plane5 vec5 5)
                           (plane6 vec6 6)
                           plane7 plane8 plane9 plane10 plane11 plane12)
      (with-graphene-frustum (frustum plane1 plane2 plane3 plane4 plane5 plane6)

        (is-false (list plane1 plane2 plane3
                        plane4 plane5 plane6))


        (is-false (frustum-planes frustum
                                  (list plane7 plane8 plane9
                                        plane10 plane11 plane12)))

;      (is (pointer-eq plane1
;                      (first (frustum-planes frustum
;                                             (list plane1 plane2 plane3
;                                                   plane4 plane5 plane6)))))
))))

;;;     graphene_frustum_contains_point
;;;     graphene_frustum_intersects_sphere
;;;     graphene_frustum_intersects_box
;;;     graphene_frustum_equal

;;; 2022-9-26
