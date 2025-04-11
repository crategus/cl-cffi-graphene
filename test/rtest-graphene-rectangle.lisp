(in-package :graphene-test)

(def-suite graphene-rectangle :in graphene-suite)
(in-suite graphene-rectangle)

;;; --- Types and Values -------------------------------------------------------

;;;     graphene_rect_t

(test graphene-rect-structure
  (is (= 16 (cffi:foreign-type-size '(:struct graphene:rect-t))))
  (is (equal '(graphene::origin graphene::size)
             (cffi:foreign-slot-names '(:struct graphene:rect-t))))
  (cffi:with-foreign-object (rect '(:struct graphene:rect-t))
    (is (cffi:pointerp (graphene:rect-init rect 1 2 3 4)))
    (is (cffi:pointerp (graphene:rect-origin rect)))
    (is (= 1 (graphene:point-x (graphene:rect-origin rect))))
    (is (= 2 (graphene:point-y (graphene:rect-origin rect))))
    (is (cffi:pointerp (graphene:rect-size rect)))
    (is (= 3 (graphene:size-width (graphene:rect-size rect))))
    (is (= 4 (graphene:size-height (graphene:rect-size rect))))))

;;; --- Macros -----------------------------------------------------------------

;;;     graphene:with-rect

(test graphene-with-rect.1
  (graphene:with-rect (r)
    (is (= 0.0 (graphene:rect-x r)))
    (is (= 0.0 (graphene:rect-y r)))
    (is (= 0.0 (graphene:rect-width r)))
    (is (= 0.0 (graphene:rect-height r)))))

(test graphene-with-rect.2
  (graphene:with-rect (r 1 2 3 4)
    (is (= 1.0 (graphene:rect-x r)))
    (is (= 2.0 (graphene:rect-y r)))
    (is (= 3.0 (graphene:rect-width r)))
    (is (= 4.0 (graphene:rect-height r)))))

(test graphene-with-rect.3
  (graphene:with-rects ((r1 1 2 3 4) (r r1))
    (is (= 1.0 (graphene:rect-x r)))
    (is (= 2.0 (graphene:rect-y r)))
    (is (= 3.0 (graphene:rect-width r)))
    (is (= 4.0 (graphene:rect-height r)))))

(test graphene-with-rect.4
  (graphene:with-rects ((r1 1 2 3 4) (r (r1 graphene:rect-t)))
    (is (= 1.0 (graphene:rect-x r)))
    (is (= 2.0 (graphene:rect-y r)))
    (is (= 3.0 (graphene:rect-width r)))
    (is (= 4.0 (graphene:rect-height r)))))

(test graphene-with-rect.5
  (graphene:with-rect (r (graphene:rect-zero))
    (is (= 0.0 (graphene:rect-x r)))
    (is (= 0.0 (graphene:rect-y r)))
    (is (= 0.0 (graphene:rect-width r)))
    (is (= 0.0 (graphene:rect-height r)))))

(test graphene-with-rect.6
  (graphene:with-rect (r ((graphene:rect-zero) graphene:rect-t))
    (is (= 0.0 (graphene:rect-x r)))
    (is (= 0.0 (graphene:rect-y r)))
    (is (= 0.0 (graphene:rect-width r)))
    (is (= 0.0 (graphene:rect-height r)))))

(test graphene-with-rect.7
  (graphene:with-rect (r1)
    (graphene:with-rect (r (graphene:rect-init r1 1 2 3 4))
      (is (= 1.0 (graphene:rect-x r)))
      (is (= 2.0 (graphene:rect-y r)))
      (is (= 3.0 (graphene:rect-width r)))
      (is (= 4.0 (graphene:rect-height r))))))

;;;     graphene:with-rects

(test graphene-with-rects
  (graphene:with-rects (r1 (r2 1 2 3 4) (r3 r2) (r4 (r2 graphene:rect-t)))
      (is (= 0.0 (graphene:rect-x r1)))
      (is (= 0.0 (graphene:rect-y r1)))
      (is (= 0.0 (graphene:rect-width r1)))
      (is (= 0.0 (graphene:rect-height r1)))
      (is (= 1.0 (graphene:rect-x r2)))
      (is (= 2.0 (graphene:rect-y r2)))
      (is (= 3.0 (graphene:rect-width r2)))
      (is (= 4.0 (graphene:rect-height r2)))
      (is (= 1.0 (graphene:rect-x r3)))
      (is (= 2.0 (graphene:rect-y r3)))
      (is (= 3.0 (graphene:rect-width r3)))
      (is (= 4.0 (graphene:rect-height r3)))
      (is (= 1.0 (graphene:rect-x r4)))
      (is (= 2.0 (graphene:rect-y r4)))
      (is (= 3.0 (graphene:rect-width r4)))
      (is (= 4.0 (graphene:rect-height r4)))))

;;; --- Functions --------------------------------------------------------------

(test graphene-rect-x/y/with/height
  (graphene:with-rect (r 1 2 3 4)
    (is (= 1.0 (graphene:rect-x r)))
    (is (= 1.0 (graphene:point-x (graphene:rect-origin r))))
    (is (= 2.0 (graphene:rect-y r)))
    (is (= 2.0 (graphene:point-y (graphene:rect-origin r))))
    (is (= 3.0 (graphene:rect-width r)))
    (is (= 3.0 (graphene:size-width (graphene:rect-size r))))
    (is (= 4.0 (graphene:rect-height r)))
    (is (= 4.0 (graphene:size-height (graphene:rect-size r))))

    (is (= 1.5 (setf (graphene:point-x (graphene:rect-origin r)) 1.5)))
    (is (= 2.5 (setf (graphene:point-y (graphene:rect-origin r)) 2.5)))
    (is (= 3.5 (setf (graphene:size-width (graphene:rect-size r)) 3.5)))
    (is (= 4.5 (setf (graphene:size-height (graphene:rect-size r)) 4.5)))

    (is (= 1.5 (graphene:rect-x r)))
    (is (= 1.5 (graphene:point-x (graphene:rect-origin r))))
    (is (= 2.5 (graphene:rect-y r)))
    (is (= 2.5 (graphene:point-y (graphene:rect-origin r))))
    (is (= 3.5 (graphene:rect-width r)))
    (is (= 3.5 (graphene:size-width (graphene:rect-size r))))
    (is (= 4.5 (graphene:rect-height r)))
    (is (= 4.5 (graphene:size-height (graphene:rect-size r))))))

;;;     graphene_rect_alloc
;;;     graphene_rect_free

(test graphene-rect-alloc/free
  (let (rect)
    (is (cffi:pointerp (setf rect (graphene:rect-alloc))))
    (is (cffi:pointerp (setf rect (graphene:rect-init rect 0.0 1.0 3.0 4.0))))

    (is (cffi:pointerp (graphene:rect-origin rect)))
    (is (= 0.0 (graphene:point-x (graphene:rect-origin rect))))
    (is (= 1.0 (graphene:point-y (graphene:rect-origin rect))))

    (is (cffi:pointerp (graphene:rect-size rect)))
    (is (= 3.0 (graphene:size-width (graphene:rect-size rect))))
    (is (= 4.0 (graphene:size-height (graphene:rect-size rect))))

    (is (= 0.0 (graphene:rect-x rect)))
    (is (= 1.0 (graphene:rect-y rect)))
    (is (= 3.0 (graphene:rect-width rect)))
    (is (= 4.0 (graphene:rect-height rect)))

    (is-false (graphene:rect-free rect))))

;;;     graphene_rect_init

(test graphene-rect-init.1
  (graphene:with-rect (rect)
    (is (cffi:pointerp (setf rect (graphene:rect-init rect 1.0 2.0 3.0 4.0))))
    (is (= 1.0 (graphene:rect-x rect)))
    (is (= 1.0 (graphene:point-x (graphene:rect-origin rect))))
    (is (= 2.0 (graphene:rect-y rect)))
    (is (= 2.0 (graphene:point-y (graphene:rect-origin rect))))

    (is (cffi:pointerp (setf rect (graphene:rect-init rect 1.0 2.0 1 1/2))))
    (is (= 1.0 (graphene:rect-width rect)))
    (is (= 1.0 (graphene:size-width (graphene:rect-size rect))))
    (is (= 0.5 (graphene:rect-height rect)))
    (is (= 0.5 (graphene:size-height (graphene:rect-size rect))))

    (is (cffi:pointerp (setf rect (graphene:rect-init rect 2.5d0 3.5d0 3 4))))
    (is (= 2.5 (graphene:rect-x rect)))
    (is (= 2.5 (graphene:point-x (graphene:rect-origin rect))))
    (is (= 3.5 (graphene:rect-y rect)))
    (is (= 3.5 (graphene:point-y (graphene:rect-origin rect))))))

(test graphene-rect-init.2
  (graphene:with-rect (rect 1.0 2.0 3.0 4.0)
    (is (= 1.0 (graphene:rect-x rect)))
    (is (= 2.0 (graphene:rect-y rect)))
    (is (= 3.0 (graphene:rect-width rect)))
    (is (= 4.0 (graphene:rect-height rect)))

    (is (= 1.0 (graphene:point-x (graphene:rect-origin rect))))
    (is (= 2.0 (graphene:point-y (graphene:rect-origin rect))))
    (is (= 3.0 (graphene:size-width (graphene:rect-size rect))))
    (is (= 4.0 (graphene:size-height (graphene:rect-size rect))))))

(test graphene-rect-init.3
  (graphene:with-rects ((rect1 1.0 2.0 3.0 4.0) rect2)
    (is (= 3.0 (graphene:rect-width rect1)))
    (is (= 4.0 (graphene:rect-height rect1)))
    (is (= 0.0 (graphene:rect-width rect2)))
    (is (= 0.0 (graphene:rect-height rect2)))))

;;;     graphene_rect_init_from_rect

(test graphene-rect-init-from-rect
  (graphene:with-rects ((rect1 1.0 2.0 3.0 4.0) rect2)
    (is (cffi:pointerp (setf rect2 (graphene:rect-init-from-rect rect2 rect1))))
    (is (= 1.0 (graphene:rect-x rect2)))
    (is (= 2.0 (graphene:rect-y rect2)))
    (is (= 3.0 (graphene:rect-width rect2)))
    (is (= 4.0 (graphene:rect-height rect2)))))

;;;     graphene_rect_equal

(test graphene-rect-equal
  (graphene:with-rects ((rect1 1.0 2.0 3.0 4.0)
                        (rect2 1 2 3 4)
                        (rect3 0 0 0 0))
    (is-true (graphene:rect-equal rect1 rect2))
    (is-false (graphene:rect-equal rect1 rect3))
    (is-false (graphene:rect-equal rect2 rect3))))

;;;     graphene_rect_normalize
;;;     graphene_rect_normalize_r

(test graphene-rect-normalize
  (graphene:with-rects ((rect -4 -3 -2 -1) result)
    (graphene:rect-normalize rect result)
    (is (= -6.0 (graphene:rect-x result)))
    (is (= -4.0 (graphene:rect-y result)))
    (is (= 2.0 (graphene:rect-width result)))
    (is (= 1.0 (graphene:rect-height result)))))

;;;     graphene_rect_get_center

(test graphene-rect-center
  (graphene:with-rect (rect 1.0 2.0 3.0 4.0)
    (graphene:with-point (p)
      (is (cffi:pointerp (setf p (graphene:rect-center rect p))))
      (is (= 2.5 (graphene:point-x p)))
      (is (= 4.0 (graphene:point-y p))))))

;;;     graphene_rect_get_top_left

(test graphene-rect-top-left
  (graphene:with-rect (rect 1.0 2.0 3.0 4.0)
    (graphene:with-point (p)
      (is (cffi:pointerp (setf p (graphene:rect-top-left rect p))))
      (is (= 1.0 (graphene:point-x p)))
      (is (= 2.0 (graphene:point-y p))))))

;;;     graphene_rect_get_top_right

(test graphene-rect-top-right
  (graphene:with-rect (rect 1.0 2.0 3.0 4.0)
    (graphene:with-point (p)
      (is (cffi:pointerp (setf p (graphene:rect-top-right rect p))))
      (is (= 4.0 (graphene:point-x p)))
      (is (= 2.0 (graphene:point-y p))))))

;;;     graphene_rect_get_bottom_right

(test graphene-rect-bottom-right
  (graphene:with-rect (rect 1.0 2.0 3.0 4.0)
    (graphene:with-point (p)
      (is (cffi:pointerp (setf p (graphene:rect-bottom-right rect p))))
      (is (= 4.0 (graphene:point-x p)))
      (is (= 6.0 (graphene:point-y p))))))

;;;     graphene_rect_get_bottom_left

(test graphene-rect-bottom-left
  (graphene:with-rect (rect 1.0 2.0 3.0 4.0)
    (graphene:with-point (p)
      (is (cffi:pointerp (setf p (graphene:rect-bottom-left rect p))))
      (is (= 1.0 (graphene:point-x p)))
      (is (= 6.0 (graphene:point-y p))))))

;;;     graphene_rect_get_x
;;;     graphene_rect_get_y
;;;     graphene_rect_get_width
;;;     graphene_rect_get_height

(test graphene-rect-x/y/width/height
  (graphene:with-rect (rect 1.0 2.0 3.0 4.0)
    (is (= 1.0 (graphene:rect-x rect)))
    (is (= 2.0 (graphene:rect-y rect)))
    (is (= 3.0 (graphene:rect-width rect)))
    (is (= 4.0 (graphene:rect-height rect)))))

;;;     graphene_rect_get_area

(test graphene-rect-area
  (graphene:with-rect (rect 1.0 2.0 3.0 4.0)
    (is (= 12.0 (graphene:rect-area rect)))))

;;;     graphene_rect_get_vertices

(test graphene-rect-vertices
  (let (result)
    (graphene:with-vec2s (v1 v2 v3 v4)
      (graphene:with-rect (rect 1.0 2.0 3.0 4.0)
        (is (every #'cffi:pointerp
                   (setf result
                         (graphene:rect-vertices rect (list v1 v2 v3 v4)))))
        (is (cffi:pointer-eq v1 (first result)))
        (is (equal '(1.0 2.0) (graphene:vec2-to-float (first result))))
        (is (cffi:pointer-eq v2 (second result)))
        (is (equal '(4.0 2.0) (graphene:vec2-to-float (second result))))
        (is (cffi:pointer-eq v3 (third result)))
        (is (equal '(4.0 6.0) (graphene:vec2-to-float (third result))))
        (is (cffi:pointer-eq v4 (fourth result)))
        (is (equal '(1.0 6.0) (graphene:vec2-to-float (fourth result))))))))

;;;     graphene_rect_union

(test graphene-rect-union
  (graphene:with-rects ((rect1 0 0 4 2) (rect2 2 1 4 2) result)
    (is (cffi:pointerp (setf result (graphene:rect-union rect1 rect2 result))))
    (is (= 0.0 (graphene:rect-x result)))
    (is (= 0.0 (graphene:rect-y result)))
    (is (= 6.0 (graphene:rect-width result)))
    (is (= 3.0 (graphene:rect-height result)))))

;;;     graphene_rect_intersection

(test graphene-rect-intersection
  (graphene:with-rects ((rect1 0 0 4 2) (rect2 2 1 4 2) result)
    (is (cffi:pointerp (setf result
                             (graphene:rect-intersection rect1 rect2 result))))
    (is (= 2.0 (graphene:rect-x result)))
    (is (= 1.0 (graphene:rect-y result)))
    (is (= 2.0 (graphene:rect-width result)))
    (is (= 1.0 (graphene:rect-height result)))))

;;;     graphene_rect_contains_point

(test graphene-rect-contains-point
  (graphene:with-rect (rect 0 0 4 2)
    (graphene:with-points ((p1 1 1) (p2 5 1) (p3 1 3))
      (is-true (graphene:rect-contains-point rect p1))
      (is-false (graphene:rect-contains-point rect p2))
      (is-false (graphene:rect-contains-point rect p3)))))

;;;     graphene_rect_contains_rect

(test graphene-rect-contains-rect
  (graphene:with-rects ((rect1 0 0 4 2) (rect2 1 1 1 1) (rect3 1 1 4 2))
    (is-true (graphene:rect-contains-rect rect1 rect2))
    (is-false (graphene:rect-contains-rect rect1 rect3))))

;;;     graphene_rect_offset
;;;     graphene_rect_offset_r

(test graphene-rect-offset
  (graphene:with-rects ((rect 1 2 3 4) result)
    (graphene:rect-offset rect 0.5 0.5 result)
    (is (= 1.5 (graphene:rect-x result)))
    (is (= 2.5 (graphene:rect-y result)))
    (is (= 3.0 (graphene:rect-width result)))
    (is (= 4.0 (graphene:rect-height result)))))

;;;     graphene_rect_inset
;;;     graphene_rect_inset_r

(test graphene-rect-inset.1
  (graphene:with-rects ((rect 1 2 3 4) result)
    (graphene:rect-inset rect 0.5 0.5 result)
    (is (= 1.5 (graphene:rect-x result)))
    (is (= 2.5 (graphene:rect-y result)))
    (is (= 2.0 (graphene:rect-width result)))
    (is (= 3.0 (graphene:rect-height result)))))

(test graphene-rect-inset.2
  (graphene:with-rects ((rect 1 2 3 4) result)
    (graphene:rect-inset rect -0.5 -0.5 result)
    (is (= 0.5 (graphene:rect-x result)))
    (is (= 1.5 (graphene:rect-y result)))
    (is (= 4.0 (graphene:rect-width result)))
    (is (= 5.0 (graphene:rect-height result)))))

;;;     graphene_rect_round_extents

(test graphene-rect-round-extents
  (graphene:with-rects ((rect 0.5 1.5 2.5 3.5) result)
    (graphene:rect-round-extents rect result)
    (is (= 0.0 (graphene:rect-x result)))
    (is (= 1.0 (graphene:rect-y result)))
    (is (= 3.0 (graphene:rect-width result)))
    (is (= 4.0 (graphene:rect-height result)))))

;;;     graphene_rect_expand

(test graphene-rect-expand
  (graphene:with-rects ((rect 0 0 5 5) result)
    (graphene:with-point (point 10 20)
      (graphene:rect-expand rect point result)
      (is (=  0.0 (graphene:rect-x result)))
      (is (=  0.0 (graphene:rect-y result)))
      (is (= 10.0 (graphene:rect-width result)))
      (is (= 20.0 (graphene:rect-height result))))))

;;;     graphene_rect_interpolate

(test graphene-rect-interpolate
  (graphene:with-rects ((rect1 0 0 5 5) (rect2 2 3 10 20) result)
    (graphene:rect-interpolate rect1 rect2 0.5 result)
    (is (=  1.0 (graphene:rect-x result)))
    (is (=  1.5 (graphene:rect-y result)))
    (is (=  7.5 (graphene:rect-width result)))
    (is (= 12.5 (graphene:rect-height result)))))

;;;     graphene_rect_zero

(test graphene-rect-zero
  (graphene:with-rect (rect)
    (graphene:rect-init-from-rect rect (graphene:rect-zero))
    (is (= 0.0 (graphene:rect-x rect)))
    (is (= 0.0 (graphene:rect-y rect)))
    (is (= 0.0 (graphene:rect-width rect)))
    (is (= 0.0 (graphene:rect-height rect)))))

;;;     graphene_rect_scale

(test graphene-rect-scale
  (graphene:with-rects ((rect 1 2 3 4) result)
    (graphene:rect-scale rect 2 3 result)
      (is (=  2.0 (graphene:rect-x result)))
      (is (=  6.0 (graphene:rect-y result)))
      (is (=  6.0 (graphene:rect-width result)))
      (is (= 12.0 (graphene:rect-height result)))))

;;; 2025-4-3
