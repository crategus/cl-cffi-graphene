(in-package :graphene-test)

(def-suite graphene-rectangle :in graphene-suite)
(in-suite graphene-rectangle)

;;; --- Types and Values -------------------------------------------------------

;;;     graphene_rect_t

;;; --- Macros -----------------------------------------------------------------

;;;     with-graphene-rect

(test with-graphene-rect.1
  (with-graphene-rect (r)
    (is (= 0.0 (rect-x r)))
    (is (= 0.0 (rect-y r)))
    (is (= 0.0 (rect-width r)))
    (is (= 0.0 (rect-height r)))))

(test with-graphene-rect.2
  (with-graphene-rect (r 1 2 3 4)
    (is (= 1.0 (rect-x r)))
    (is (= 2.0 (rect-y r)))
    (is (= 3.0 (rect-width r)))
    (is (= 4.0 (rect-height r)))))

(test with-graphene-rect.3
  (with-graphene-rects ((r1 1 2 3 4) (r r1))
    (is (= 1.0 (rect-x r)))
    (is (= 2.0 (rect-y r)))
    (is (= 3.0 (rect-width r)))
    (is (= 4.0 (rect-height r)))))

(test with-graphene-rect.4
  (with-graphene-rects ((r1 1 2 3 4) (r (r1 rect-t)))
    (is (= 1.0 (rect-x r)))
    (is (= 2.0 (rect-y r)))
    (is (= 3.0 (rect-width r)))
    (is (= 4.0 (rect-height r)))))

;;;     with-graphene-rects

(test with-graphene-rects
  (with-graphene-rects (r1 (r2 1 2 3 4) (r3 r2) (r4 (r2 rect-t)))
      (is (= 0.0 (rect-x r1)))
      (is (= 0.0 (rect-y r1)))
      (is (= 0.0 (rect-width r1)))
      (is (= 0.0 (rect-height r1)))
      (is (= 1.0 (rect-x r2)))
      (is (= 2.0 (rect-y r2)))
      (is (= 3.0 (rect-width r2)))
      (is (= 4.0 (rect-height r2)))
      (is (= 1.0 (rect-x r3)))
      (is (= 2.0 (rect-y r3)))
      (is (= 3.0 (rect-width r3)))
      (is (= 4.0 (rect-height r3)))
      (is (= 1.0 (rect-x r4)))
      (is (= 2.0 (rect-y r4)))
      (is (= 3.0 (rect-width r4)))
      (is (= 4.0 (rect-height r4)))))

;;; --- Functions --------------------------------------------------------------

(test rect-x/y/with/height
  (with-graphene-rect (r 1 2 3 4)
    (is (= 1.0 (rect-x r)))
    (is (= 1.0 (point-x (rect-origin r))))
    (is (= 2.0 (rect-y r)))
    (is (= 2.0 (point-y (rect-origin r))))
    (is (= 3.0 (rect-width r)))
    (is (= 3.0 (size-width (rect-size r))))
    (is (= 4.0 (rect-height r)))
    (is (= 4.0 (size-height (rect-size r))))

    (is (= 1.5 (setf (point-x (rect-origin r)) 1.5)))
    (is (= 2.5 (setf (point-y (rect-origin r)) 2.5)))
    (is (= 3.5 (setf (size-width (rect-size r)) 3.5)))
    (is (= 4.5 (setf (size-height (rect-size r)) 4.5)))

    (is (= 1.5 (rect-x r)))
    (is (= 1.5 (point-x (rect-origin r))))
    (is (= 2.5 (rect-y r)))
    (is (= 2.5 (point-y (rect-origin r))))
    (is (= 3.5 (rect-width r)))
    (is (= 3.5 (size-width (rect-size r))))
    (is (= 4.5 (rect-height r)))
    (is (= 4.5 (size-height (rect-size r))))))

;;;     graphene_rect_alloc
;;;     graphene_rect_free

(test rect-alloc
  (let ((rect nil))
    (is (pointerp (setf rect (rect-alloc))))
    (is (pointerp (setf rect (rect-init rect 0.0 1.0 3.0 4.0))))

    (is (pointerp (rect-origin rect)))
    (is (= 0.0 (point-x (rect-origin rect))))
    (is (= 1.0 (point-y (rect-origin rect))))

    (is (pointerp (rect-size rect)))
    (is (= 3.0 (size-width (rect-size rect))))
    (is (= 4.0 (size-height (rect-size rect))))

    (is (= 0.0 (rect-x rect)))
    (is (= 1.0 (rect-y rect)))
    (is (= 3.0 (rect-width rect)))
    (is (= 4.0 (rect-height rect)))

    (is-false (rect-free rect))))

;;;     graphene_rect_init

(test rect-init.1
  (with-graphene-rect (rect)
    (is (pointerp (setf rect (rect-init rect 1.0 2.0 3.0 4.0))))
    (is (= 1.0 (rect-x rect)))
    (is (= 1.0 (point-x (rect-origin rect))))
    (is (= 2.0 (rect-y rect)))
    (is (= 2.0 (point-y (rect-origin rect))))

    (is (pointerp (setf rect (rect-init rect 1.0 2.0 1 1/2))))
    (is (= 1.0 (rect-width rect)))
    (is (= 1.0 (size-width (rect-size rect))))
    (is (= 0.5 (rect-height rect)))
    (is (= 0.5 (size-height (rect-size rect))))

    (is (pointerp (setf rect (rect-init rect 2.5d0 3.5d0 3 4))))
    (is (= 2.5 (rect-x rect)))
    (is (= 2.5 (point-x (rect-origin rect))))
    (is (= 3.5 (rect-y rect)))
    (is (= 3.5 (point-y (rect-origin rect))))))

(test rect-init.2
  (with-graphene-rect (rect 1.0 2.0 3.0 4.0)
    (is (= 1.0 (rect-x rect)))
    (is (= 2.0 (rect-y rect)))
    (is (= 3.0 (rect-width rect)))
    (is (= 4.0 (rect-height rect)))

    (is (= 1.0 (point-x (rect-origin rect))))
    (is (= 2.0 (point-y (rect-origin rect))))
    (is (= 3.0 (size-width (rect-size rect))))
    (is (= 4.0 (size-height (rect-size rect))))))

(test rect-init.3
  (with-graphene-rects ((rect1 1.0 2.0 3.0 4.0) rect2)
    (is (= 3.0 (rect-width rect1)))
    (is (= 4.0 (rect-height rect1)))
    (is (= 0.0 (rect-width rect2)))
    (is (= 0.0 (rect-height rect2)))))

;;;     graphene_rect_init_from_rect

(test rect-init-from-rect
  (with-graphene-rects ((rect1 1.0 2.0 3.0 4.0) rect2)
    (is (pointerp (setf rect2 (rect-init-from-rect rect2 rect1))))
    (is (= 1.0 (rect-x rect2)))
    (is (= 2.0 (rect-y rect2)))
    (is (= 3.0 (rect-width rect2)))
    (is (= 4.0 (rect-height rect2)))))

;;;     graphene_rect_equal

(test rect-equal
  (with-graphene-rects ((rect1 1.0 2.0 3.0 4.0)
                        (rect2 1 2 3 4)
                        (rect3 0 0 0 0))
    (is-true (rect-equal rect1 rect2))
    (is-false (rect-equal rect1 rect3))
    (is-false (rect-equal rect2 rect3))))

;;;     graphene_rect_normalize

(test rect-normalize
  (with-graphene-rect (rect -4 -3 -2 -1)
    (is (pointerp (setf rect (rect-normalize rect))))
    (is (= -6.0 (rect-x rect)))
    (is (= -4.0 (rect-y rect)))
    (is (= 2.0 (rect-width rect)))
    (is (= 1.0 (rect-height rect)))))

;;;     graphene_rect_normalize_r

;;;     graphene_rect_get_center

(test rect-center
  (with-graphene-rect (rect 1.0 2.0 3.0 4.0)
    (with-graphene-point (p)
      (is (pointerp (setf p (rect-center rect p))))
      (is (= 2.5 (point-x p)))
      (is (= 4.0 (point-y p))))))

;;;     graphene_rect_get_top_left

(test rect-top-left
  (with-graphene-rect (rect 1.0 2.0 3.0 4.0)
    (with-graphene-point (p)
      (is (pointerp (setf p (rect-top-left rect p))))
      (is (= 1.0 (point-x p)))
      (is (= 2.0 (point-y p))))))

;;;     graphene_rect_get_top_right

(test rect-top-right
  (with-graphene-rect (rect 1.0 2.0 3.0 4.0)
    (with-graphene-point (p)
      (is (pointerp (setf p (rect-top-right rect p))))
      (is (= 4.0 (point-x p)))
      (is (= 2.0 (point-y p))))))

;;;     graphene_rect_get_bottom_right

(test rect-bottom-right
  (with-graphene-rect (rect 1.0 2.0 3.0 4.0)
    (with-graphene-point (p)
      (is (pointerp (setf p (rect-bottom-right rect p))))
      (is (= 4.0 (point-x p)))
      (is (= 6.0 (point-y p))))))

;;;     graphene_rect_get_bottom_left

(test rect-bottom-left
  (with-graphene-rect (rect 1.0 2.0 3.0 4.0)
    (with-graphene-point (p)
      (is (pointerp (setf p (rect-bottom-left rect p))))
      (is (= 1.0 (point-x p)))
      (is (= 6.0 (point-y p))))))

;;;     graphene_rect_get_x
;;;     graphene_rect_get_y
;;;     graphene_rect_get_width
;;;     graphene_rect_get_height

(test rect-x/y/width/height
  (with-graphene-rect (rect 1.0 2.0 3.0 4.0)
    (is (= 1.0 (rect-x rect)))
    (is (= 2.0 (rect-y rect)))
    (is (= 3.0 (rect-width rect)))
    (is (= 4.0 (rect-height rect)))))

;;;     graphene_rect_get_area

(test rect-area
  (with-graphene-rect (rect 1.0 2.0 3.0 4.0)
    (is (= 12.0 (rect-area rect)))))

;;;     graphene_rect_get_vertices

;;;     graphene_rect_union

(test rect-union
  (with-graphene-rects ((rect1 0 0 4 2) (rect2 2 1 4 2) result)
    (is (pointerp (setf result (rect-union rect1 rect2 result))))
    (is (= 0.0 (rect-x result)))
    (is (= 0.0 (rect-y result)))
    (is (= 6.0 (rect-width result)))
    (is (= 3.0 (rect-height result)))))

;;;     graphene_rect_intersection

(test rect-intersection
  (with-graphene-rects ((rect1 0 0 4 2) (rect2 2 1 4 2) result)
    (is (pointerp (setf result (rect-intersection rect1 rect2 result))))
    (is (= 2.0 (rect-x result)))
    (is (= 1.0 (rect-y result)))
    (is (= 2.0 (rect-width result)))
    (is (= 1.0 (rect-height result)))))

;;;     graphene_rect_contains_point

(test rect-contains-point
  (with-graphene-rect (rect 0 0 4 2)
    (with-graphene-points ((p1 1 1) (p2 5 1) (p3 1 3))
      (is-true (rect-contains-point rect p1))
      (is-false (rect-contains-point rect p2))
      (is-false (rect-contains-point rect p3)))))

;;;     graphene_rect_contains_rect

(test rect-contains-rect
  (with-graphene-rects ((rect1 0 0 4 2) (rect2 1 1 1 1) (rect3 1 1 4 2))
    (is-true (rect-contains-rect rect1 rect2))
    (is-false (rect-contains-rect rect1 rect3))))

;;;     graphene_rect_offset
;;;     graphene_rect_offset_r
;;;     graphene_rect_inset
;;;     graphene_rect_inset_r
;;;     graphene_rect_round_to_pixel
;;;     graphene_rect_round
;;;     graphene_rect_round_extents
;;;     graphene_rect_expand
;;;     graphene_rect_interpolate
;;;     graphene_rect_zero
;;;     graphene_rect_scale

;;; 2022-9-16
